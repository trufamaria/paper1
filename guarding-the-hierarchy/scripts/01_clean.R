# 01_clean.R ---------------------------------------------------------------
# Reads the survey file, checks the columns named in config.R, reverse-codes
# items, builds the scale scores, and saves a clean analysis file.
# Also writes scale reliability (Cronbach's alpha) by country.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages(); ensure_dirs(cfg)

raw <- haven::read_sav(cfg$data_file)
message("Rows in the raw file: ", nrow(raw))

needed <- c(cfg$country_var, all_item_columns(cfg), index_columns(cfg), cfg$controls, cfg$weight_var)
check_columns(raw, needed)

# ---- Country ---------------------------------------------------------------
df <- raw
df$country <- haven::zap_labels(df[[cfg$country_var]])
df$country <- factor(df$country, levels = unname(cfg$country_codes), labels = names(cfg$country_codes))
if (anyNA(df$country)) {
  message("Dropping ", sum(is.na(df$country)), " rows whose country value is not in cfg$country_codes.")
  df <- df[!is.na(df$country), ]
}

if (!is.null(cfg$keep_if)) {
  n0 <- nrow(df)
  keep_rows <- eval(parse(text = cfg$keep_if), envir = df)
  df <- df[keep_rows %in% TRUE, ]
  message("Case filter (", cfg$keep_if, "): kept ", nrow(df), " of ", n0, " rows.")
}
message("Respondents by country:")
print(table(df$country))

# ---- Items: plain numbers, range check, reverse-coding ----------------------
df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(c(all_item_columns(cfg), index_columns(cfg), cfg$controls)),
                                      ~ as.numeric(haven::zap_labels(.x))))
for (nm in names(cfg$constructs)) {
  k <- cfg$constructs[[nm]]
  rg <- get_range(df, k)
  message(sprintf("%-45s items: %s | answer range used: %s to %s", k$label,
                  paste(k$items, collapse = ","), rg[1], rg[2]))
  if (!is.null(k$range)) {
    out_of_range <- sapply(k$items, function(v) any(df[[v]] < rg[1] | df[[v]] > rg[2], na.rm = TRUE))
    if (any(out_of_range)) {
      warning("Values outside ", rg[1], "-", rg[2], " in: ", paste(k$items[out_of_range], collapse = ", "),
              ". Check for missing-value codes such as 98 or 99 in the data.", call. = FALSE)
    }
  }
  df <- recode_reverse(df, k)
}

# ---- Categorical controls (such as gender) become 0/1 dummies ------------------
controls_used <- setdiff(cfg$controls, cfg$categorical_controls)
for (v in cfg$categorical_controls) {
  tab <- table(df[[v]], df$country)
  message("\nControl '", v, "' by country:"); print(tab)
  ref <- if (v %in% names(cfg$reference_levels)) as.character(cfg$reference_levels[[v]]) else names(which.max(rowSums(tab)))
  if (!ref %in% rownames(tab)) stop("Reference value ", ref, " for ", v, " is not in the data.", call. = FALSE)
  dummies <- character(0); small <- character(0)
  for (lv in setdiff(rownames(tab), ref)) {
    if (min(tab[lv, ]) < cfg$min_dummy_n) { small <- c(small, lv); next }
    nm <- paste0(v, "_", lv)
    df[[nm]] <- as.numeric(df[[v]] == as.numeric(lv))
    dummies <- c(dummies, nm)
  }
  controls_used <- c(controls_used, dummies)
  message("Reference category: ", ref, ". Dummies: ", paste(dummies, collapse = ", "))
  if (length(small)) {
    in_small <- df[[v]] %in% as.numeric(small)
    for (nm in dummies) df[[nm]][in_small] <- NA
    message("Categories with fewer than ", cfg$min_dummy_n, " people in a country (value ", paste(small, collapse = ", "),
            "; n = ", sum(in_small), ") are set to missing on these dummies, so the models leave them out.")
  }
}

# ---- Scale scores -----------------------------------------------------------
for (nm in names(cfg$constructs)) {
  df[[nm]] <- make_scale(df, cfg$constructs[[nm]]$items, cfg$min_valid_items)
}

# ---- Compare new scores with the scores already in the file ----------------------
for (nm in names(cfg$constructs)) {
  idx <- cfg$constructs[[nm]]$index
  if (is.null(idx)) next
  r <- stats::cor(df[[nm]], df[[idx]], use = "pairwise.complete.obs")
  message(sprintf("%-45s correlation with existing score %s: r = %.3f", cfg$constructs[[nm]]$label, idx, r))
  if (r < 0.95) warning("Low correlation for ", nm, " vs ", idx, ". Check `items` and `reverse` in config.R.", call. = FALSE)
}

# ---- Reliability by country -------------------------------------------------
rel <- purrr::map_dfr(names(cfg$constructs), function(nm) {
  k <- cfg$constructs[[nm]]
  if (length(k$items) < 2) return(NULL)
  purrr::map_dfr(c("All", levels(df$country)), function(g) {
    d <- if (g == "All") df else df[df$country == g, ]
    a <- suppressWarnings(psych::alpha(d[, k$items], check.keys = FALSE, warnings = FALSE))
    tibble::tibble(construct = k$label, group = g, items = length(k$items),
                   n = nrow(d), alpha = round(a$total$raw_alpha, 3))
  })
})
readr::write_csv(rel, file.path(cfg$tables_dir, "01_reliability.csv"))
print(rel)

# ---- Save ---------------------------------------------------------------------
keep <- unique(c("country", names(cfg$constructs), controls_used, cfg$weight_var, all_item_columns(cfg)))
saveRDS(df[, keep], cfg$derived_file)
saveRDS(controls_used, file.path(dirname(cfg$derived_file), "controls_used.rds"))
message("Saved ", nrow(df), " rows to ", cfg$derived_file, " (private: do not upload).")
