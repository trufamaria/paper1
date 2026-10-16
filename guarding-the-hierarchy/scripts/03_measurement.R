# 03_measurement.R ---------------------------------------------------------
# One-factor CFA for each multi-item scale, by country, and measurement
# invariance across countries (configural, metric, scalar).
# Skip by setting run_measurement = FALSE in config.R.
# Note: a scale with exactly 3 items is just-identified, so its fit is perfect by design.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages(); ensure_dirs(cfg)
dat <- read_clean(cfg)
run <- isTRUE(cfg$run_measurement)
if (!run) message("run_measurement is FALSE in config.R. Skipping measurement models.")

fit_cols <- c("chisq.scaled", "df.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "srmr")
get_fit <- function(fit) round(lavaan::fitMeasures(fit, fit_cols), 3)

res_cfa <- list(); res_inv <- list()
for (nm in if (run) names(cfg$constructs) else character(0)) {
  k <- cfg$constructs[[nm]]
  if (length(k$items) < 3) next
  model <- paste0("f =~ ", paste(k$items, collapse = " + "))

  # CFA by country
  for (g in levels(dat$country)) {
    f <- tryCatch(lavaan::cfa(model, data = dat[dat$country == g, ], estimator = "MLR", missing = "fiml"),
                  error = function(e) NULL)
    if (!is.null(f)) res_cfa[[paste(nm, g)]] <- c(construct = k$label, country = g, get_fit(f))
  }

  # Invariance across countries
  inv <- tryCatch({
    m1 <- lavaan::cfa(model, data = dat, group = "country", estimator = "MLR", missing = "fiml")
    m2 <- lavaan::cfa(model, data = dat, group = "country", estimator = "MLR", missing = "fiml",
                      group.equal = "loadings")
    m3 <- lavaan::cfa(model, data = dat, group = "country", estimator = "MLR", missing = "fiml",
                      group.equal = c("loadings", "intercepts"))
    out <- rbind(c(level = "configural", get_fit(m1)),
                 c(level = "metric", get_fit(m2)),
                 c(level = "scalar", get_fit(m3)))
    out <- data.frame(construct = k$label, out, check.names = FALSE)
    out$delta_cfi <- c(NA, as.numeric(out$cfi.robust[2]) - as.numeric(out$cfi.robust[1]),
                       as.numeric(out$cfi.robust[3]) - as.numeric(out$cfi.robust[2]))
    out
  }, error = function(e) { message("Invariance failed for ", k$label, ": ", conditionMessage(e)); NULL })
  if (!is.null(inv)) res_inv[[nm]] <- inv
}

if (length(res_cfa)) readr::write_csv(dplyr::bind_rows(lapply(res_cfa, as.list)),
                                      file.path(cfg$tables_dir, "03_cfa_by_country.csv"))
if (length(res_inv)) readr::write_csv(dplyr::bind_rows(res_inv),
                                      file.path(cfg$tables_dir, "03_measurement_invariance.csv"))
if (run) message("Measurement tables written. A drop in CFI larger than .01 between steps suggests the level of invariance does not hold.")
