# 02_descriptives.R --------------------------------------------------------
# Sample sizes, means, SDs, country differences, and correlations.
# Descriptives are unweighted.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages(); ensure_dirs(cfg)
dat <- read_clean(cfg)
vars <- names(cfg$constructs)
labels <- vapply(cfg$constructs, `[[`, character(1), "label")

# ---- Sample and descriptives by country --------------------------------------
desc <- purrr::map_dfr(c("All", levels(dat$country)), function(g) {
  d <- if (g == "All") dat else dat[dat$country == g, ]
  tibble::tibble(group = g, variable = labels, n = sapply(vars, function(v) sum(!is.na(d[[v]]))),
                 mean = sapply(vars, function(v) mean(d[[v]], na.rm = TRUE)),
                 sd = sapply(vars, function(v) sd(d[[v]], na.rm = TRUE)),
                 skew = sapply(vars, function(v) psych::skew(d[[v]], na.rm = TRUE)),
                 kurtosis = sapply(vars, function(v) psych::kurtosi(d[[v]], na.rm = TRUE)))
}) |> dplyr::mutate(dplyr::across(c(mean, sd, skew, kurtosis), ~ round(.x, 2)))
readr::write_csv(desc, file.path(cfg$tables_dir, "02_descriptives.csv"))

# ---- Country differences (Welch t-test, Cohen's d) ----------------------------
cmp <- purrr::map_dfr(vars, function(v) {
  g <- levels(dat$country)
  x <- dat[[v]][dat$country == g[1]]; y <- dat[[v]][dat$country == g[2]]
  tt <- t.test(x, y)
  sp <- sqrt(((sum(!is.na(x)) - 1) * var(x, na.rm = TRUE) + (sum(!is.na(y)) - 1) * var(y, na.rm = TRUE)) /
               (sum(!is.na(x)) + sum(!is.na(y)) - 2))
  tibble::tibble(variable = labels[[v]],
                 !!paste0("mean_", g[1]) := round(mean(x, na.rm = TRUE), 2),
                 !!paste0("mean_", g[2]) := round(mean(y, na.rm = TRUE), 2),
                 t = round(unname(tt$statistic), 2), df = round(unname(tt$parameter), 1),
                 p = signif(tt$p.value, 3),
                 cohens_d = round((mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) / sp, 2))
})
readr::write_csv(cmp, file.path(cfg$tables_dir, "02_country_differences.csv"))

# ---- Correlations by country ---------------------------------------------------
cors <- purrr::map_dfr(levels(dat$country), function(g) {
  d <- dat[dat$country == g, c(vars, resolve_controls(cfg))]
  ct <- psych::corr.test(d, use = "pairwise", adjust = "none")
  r <- as.data.frame(as.table(ct$r)); p <- as.data.frame(as.table(ct$p))
  names(r) <- c("var1", "var2", "r"); r$p <- p$Freq; r$country <- g
  r[as.character(r$var1) < as.character(r$var2), ]
}) |> dplyr::mutate(r = round(r, 3), p = signif(p, 3))
readr::write_csv(cors, file.path(cfg$tables_dir, "02_correlations_long.csv"))
message("Descriptives, country differences, and correlations written to ", cfg$tables_dir)
print(desc)
