# 04_mediation.R -----------------------------------------------------------
# Main analysis. Parallel mediation:
#   need for chaos -> (misogyny, authoritarianism) -> (destabilizing sharing, political violence)
# Run (a) separately in each country and (b) in one multi-group model, which
# tests whether paths and indirect effects differ between countries.
# Three specifications: main (with controls), no controls, and without political orientation.
# Unweighted models use bootstrapped 95% intervals (percentile). Weighted models
# (weight_var set in config.R) use robust ML and delta-method intervals.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages(); ensure_dirs(cfg)
dat <- read_clean(cfg)
countries <- levels(dat$country)

controls_main <- resolve_controls(cfg)
specs <- list(
  main = controls_main,
  no_controls = character(0),
  no_political_orientation = setdiff(controls_main, cfg$pol_orientation_var)
)

specs <- specs[intersect(names(specs), cfg$specs_to_run)]
stamp <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
all_params <- list(); all_tests <- list()

for (sp in names(specs)) {
  controls <- specs[[sp]]
  vars <- unique(c(cfg$predictor, cfg$mediators, cfg$outcomes, controls, cfg$weight_var, "country"))
  fiml <- identical(cfg$missing, "fiml")
  need <- if (fiml) unique(c("country", cfg$weight_var)) else vars
  d <- dat[stats::complete.cases(dat[, need]), vars]
  message("\n== Specification: ", sp, " | N = ", nrow(d), " of ", nrow(dat),
          if (fiml) " (FIML: all available answers used)" else " (listwise deletion)", " ==")

  # (a) Separate models by country
  for (g in countries) {
    dg <- d[d$country == g, ]
    stamp("fitting ", sp, " / ", g, " (", cfg$n_boot, " bootstrap draws)")
    model <- build_model(cfg$predictor, cfg$mediators, cfg$outcomes, controls, groups = NULL)
    fit <- fit_model(model, dg, cfg)
    tab <- tidy_fit(fit)
    tab$spec <- sp; tab$analysis <- paste0("separate: ", g); tab$n <- nrow(dg)
    all_params[[paste(sp, g)]] <- tab
  }

  # (b) Multi-group model with country-specific labels
  stamp("fitting ", sp, " / multi-group")
  model_mg <- build_model(cfg$predictor, cfg$mediators, cfg$outcomes, controls, groups = countries)
  fit_mg <- fit_model(model_mg, d, cfg, group = "country")
  tab <- tidy_fit(fit_mg)
  tab$spec <- sp; tab$analysis <- "multigroup"; tab$n <- nrow(d)
  all_params[[paste(sp, "mg")]] <- tab
  r2 <- lavaan::lavInspect(fit_mg, "rsquare")
  readr::write_csv(
    dplyr::bind_rows(lapply(names(r2), function(g) tibble::tibble(group = g, variable = names(r2[[g]]), r2 = round(unname(r2[[g]]), 3)))),
    file.path(cfg$tables_dir, paste0("04_r2_", sp, ".csv")))

  # (c) Do all structural paths differ across countries? (equal vs free paths)
  model_eq <- build_model(cfg$predictor, cfg$mediators, cfg$outcomes, controls, groups = NULL)
  lrt <- tryCatch({
    free <- lavaan::sem(model_mg, data = d, group = "country", meanstructure = TRUE, fixed.x = !fiml,
                        missing = if (fiml) "fiml" else "listwise",
                        estimator = if (is.null(cfg$weight_var)) "ML" else "MLR",
                        sampling.weights = cfg$weight_var)
    eq <- lavaan::sem(model_eq, data = d, group = "country", meanstructure = TRUE, fixed.x = !fiml,
                      missing = if (fiml) "fiml" else "listwise",
                      estimator = if (is.null(cfg$weight_var)) "ML" else "MLR",
                      sampling.weights = cfg$weight_var)
    a <- lavaan::lavTestLRT(free, eq)
    tibble::tibble(spec = sp, test = "Equal vs free structural paths across countries",
                   chisq_diff = round(a$`Chisq diff`[2], 3), df_diff = a$`Df diff`[2],
                   p = signif(a$`Pr(>Chisq)`[2], 3))
  }, error = function(e) { message("LRT failed: ", conditionMessage(e)); NULL })
  if (!is.null(lrt)) all_tests[[sp]] <- lrt
}

params <- dplyr::bind_rows(all_params)
num <- vapply(params, is.numeric, logical(1))
params[num] <- lapply(params[num], round, 4)
keep <- c("spec", "analysis", "n", "lhs", "op", "rhs", "label", "group", "est", "se", "pvalue",
          "ci.lower", "ci.upper", "std.all")
params <- params[, intersect(keep, names(params))]
readr::write_csv(params, file.path(cfg$tables_dir, "04_all_parameters.csv"))

# Indirect, total, and difference effects in one short table
effects <- params |> dplyr::filter(op == ":=")
readr::write_csv(effects, file.path(cfg$tables_dir, "04_indirect_effects.csv"))
if (length(all_tests)) readr::write_csv(dplyr::bind_rows(all_tests), file.path(cfg$tables_dir, "04_path_equality_tests.csv"))

message("\nMain-specification indirect effects:")
print(effects |> dplyr::filter(spec == "main", analysis == "multigroup") |>
        dplyr::select(label, est, ci.lower, ci.upper, pvalue), n = 60)
message("Interpretation: an indirect effect is supported when its 95% interval excludes zero.")
