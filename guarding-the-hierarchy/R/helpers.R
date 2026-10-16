# R/helpers.R -------------------------------------------------------------
# Small functions shared by the scripts. Nothing here runs by itself.
# -------------------------------------------------------------------------

load_packages <- function() {
  pkgs <- c("haven", "dplyr", "tidyr", "purrr", "tibble", "stringr", "readr",
            "psych", "lavaan", "ggplot2")
  need <- setdiff(pkgs, rownames(installed.packages()))
  if (length(need)) install.packages(need)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

ensure_dirs <- function(cfg) {
  for (d in c(cfg$tables_dir, cfg$figures_dir, dirname(cfg$derived_file))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

# Stop early, with a clear message, if config.R names a column that is not in the data.
# Suggests similar names for each missing column and lists every column in the file.
check_columns <- function(df, cols, what = "the data file") {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    hints <- vapply(missing, function(m) {
      close <- unique(c(names(df)[grepl(substr(tolower(m), 1, 4), tolower(names(df)), fixed = TRUE)],
                        agrep(tolower(m), names(df), max.distance = 0.3, ignore.case = TRUE, value = TRUE)))
      if (length(close)) paste0("  ", m, "  ->  maybe: ", paste(head(close, 6), collapse = ", ")) else paste0("  ", m, "  ->  no similar name")
    }, character(1))
    stop("config.R names columns that are not in ", what, ":\n", paste(hints, collapse = "\n"),
         "\n\nAll columns in the file:\n  ", paste(names(df), collapse = ", "), call. = FALSE)
  }
}

all_item_columns <- function(cfg) unique(unlist(lapply(cfg$constructs, `[[`, "items")))
index_columns <- function(cfg) unique(unlist(lapply(cfg$constructs, `[[`, "index")))

# Lowest and highest answer: from config.R, or read from the data when range is NULL.
get_range <- function(df, construct) {
  if (!is.null(construct$range)) return(construct$range)
  range(unlist(df[, construct$items, drop = FALSE]), na.rm = TRUE)
}

# Control columns actually used in the models (dummies replace categorical controls).
resolve_controls <- function(cfg) {
  f <- file.path(dirname(cfg$derived_file), "controls_used.rds")
  if (!file.exists(f)) stop("Cannot find ", f, ". Run scripts/01_clean.R first.", call. = FALSE)
  readRDS(f)
}

# Read the clean file made by scripts/01_clean.R, with a clear message if it is missing.
read_clean <- function(cfg) {
  if (!file.exists(cfg$derived_file)) {
    stop("Cannot find ", cfg$derived_file, ". Run scripts/01_clean.R first (or source('run_all.R')). ",
         "If step 1 stopped with an error, fix the column names in config.R and run it again.", call. = FALSE)
  }
  readRDS(cfg$derived_file)
}

# Reverse-code the listed items inside the data frame.
recode_reverse <- function(df, construct) {
  rg <- get_range(df, construct)
  for (v in construct$reverse) df[[v]] <- sum(rg) - df[[v]]
  df
}

# Average of the items. Missing if the person answered fewer than min_prop of them.
make_scale <- function(df, items, min_prop = 0.8) {
  m <- as.matrix(df[, items, drop = FALSE])
  storage.mode(m) <- "double"
  score <- rowMeans(m, na.rm = TRUE)
  score[rowMeans(!is.na(m)) < min_prop] <- NA_real_
  score
}

# Names for the model: country labels become suffixes (for example a_misogyny_us).
group_suffix <- function(groups) tolower(gsub("[^A-Za-z0-9]", "", groups))

# Write lavaan syntax for a parallel mediation model with observed scores.
#   predictor -> mediators -> outcomes, plus a direct path and controls.
# With `groups` (two country names) every path gets its own label per country,
# so paths and indirect effects can be compared across countries.
# With groups = NULL and a multi-group fit, same labels mean equal paths (used for the equality test).
build_model <- function(pred, meds, outs, controls, groups = NULL) {
  lab <- function(base) {
    if (is.null(groups)) return(base)
    paste0("c(", paste0(base, "_", group_suffix(groups), collapse = ", "), ")")
  }
  ctrl <- if (length(controls)) paste0(" + ", paste(controls, collapse = " + ")) else ""
  lines <- character(0)

  for (m in meds) {
    lines <- c(lines, sprintf("%s ~ %s*%s%s", m, lab(paste0("a_", m)), pred, ctrl))
  }
  for (o in outs) {
    paths <- c(sprintf("%s*%s", lab(paste0("cp_", o)), pred),
               sapply(meds, function(m) sprintf("%s*%s", lab(paste0("b_", m, "_", o)), m)))
    lines <- c(lines, sprintf("%s ~ %s%s", o, paste(paths, collapse = " + "), ctrl))
  }
  if (length(meds) > 1) lines <- c(lines, sprintf("%s ~~ %s", meds[1], paste(meds[-1], collapse = " + ")))
  if (length(outs) > 1) lines <- c(lines, sprintf("%s ~~ %s", outs[1], paste(outs[-1], collapse = " + ")))

  # Defined parameters: indirect, total indirect, and total effects
  sfx <- if (is.null(groups)) "" else paste0("_", group_suffix(groups))
  for (s in sfx) {
    for (o in outs) {
      ind_names <- character(0)
      for (m in meds) {
        nm <- paste0("ind_", m, "_", o, s)
        lines <- c(lines, sprintf("%s := a_%s%s * b_%s_%s%s", nm, m, s, m, o, s))
        ind_names <- c(ind_names, nm)
      }
      lines <- c(lines,
                 sprintf("total_ind_%s%s := %s", o, s, paste(ind_names, collapse = " + ")),
                 sprintf("total_%s%s := cp_%s%s + total_ind_%s%s", o, s, o, s, o, s))
    }
  }
  # Country differences (first group minus second group)
  if (!is.null(groups) && length(groups) == 2) {
    s1 <- paste0("_", group_suffix(groups[1])); s2 <- paste0("_", group_suffix(groups[2]))
    for (o in outs) {
      for (m in meds) {
        lines <- c(lines, sprintf("diff_ind_%s_%s := ind_%s_%s%s - ind_%s_%s%s", m, o, m, o, s1, m, o, s2))
      }
      lines <- c(lines, sprintf("diff_total_ind_%s := total_ind_%s%s - total_ind_%s%s", o, o, s1, o, s2))
    }
  }
  paste(lines, collapse = "\n")
}

# Fit with bootstrapped intervals (no weights) or robust ML (with weights).
fit_model <- function(model, data, cfg, group = NULL) {
  fiml <- identical(cfg$missing, "fiml")
  args <- list(model = model, data = data, meanstructure = TRUE, fixed.x = !fiml)
  if (fiml) args$missing <- "fiml"
  if (!is.null(group)) args$group <- group
  if (is.null(cfg$weight_var)) {
    args$se <- "bootstrap"; args$bootstrap <- cfg$n_boot
    if (cfg$n_cores > 1 && .Platform$OS.type != "windows") {
      args$parallel <- "multicore"; args$ncpus <- cfg$n_cores
    }
  } else {
    args$estimator <- "MLR"; args$sampling.weights <- cfg$weight_var
  }
  RNGkind("L'Ecuyer-CMRG")   # keeps parallel bootstrap results reproducible
  set.seed(cfg$seed)
  do.call(lavaan::sem, args)
}

# Tidy table of parameter estimates with confidence intervals.
tidy_fit <- function(fit) {
  est <- if (lavInspect(fit, "options")$se == "bootstrap") {
    lavaan::parameterEstimates(fit, ci = TRUE, level = 0.95, boot.ci.type = "perc", standardized = TRUE)
  } else {
    lavaan::parameterEstimates(fit, ci = TRUE, level = 0.95, standardized = TRUE)
  }
  tibble::as_tibble(est)
}

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")
}
country_colors <- function(cfg) {
  stats::setNames(c("#2a6f97", "#2d6a4f"), names(cfg$country_codes))
}
