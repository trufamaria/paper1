# 03_measurement.R ---------------------------------------------------------
# Four-factor measurement model (need for chaos, misogyny, authoritarianism, hostile
# content sharing) across Brazil and the U.S. Same steps as the manuscript:
#   1. configural model without residual correlations
#   2. configural model with the five within-construct residual correlations
#   3. the same model in each country
#   4. metric model (loadings equal across countries) and the comparison with step 2
# Writes 03_measurement_fit.csv and Table A2 (standardized loadings, metric model).
# MLR estimation with FIML, as in the manuscript.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); source("R/paper_model.R"); load_packages(); ensure_dirs(cfg)
dat <- read_paper_data(cfg)
cols <- unlist(lapply(latent_constructs(cfg), function(v) cfg$constructs[[v]]$items))
d <- dat[, c("country", cols)]

est <- function(model, data = d, ...) lavaan::cfa(model, data = data, estimator = "MLR", missing = "fiml", ...)
gl <- levels(d$country)
m0 <- measurement_syntax(cfg, residuals = FALSE)
m1 <- measurement_syntax(cfg, residuals = TRUE)
f_conf0 <- est(m0, group = "country", group.label = gl)
f_conf  <- est(m1, group = "country", group.label = gl)
f_metric <- est(m1, group = "country", group.label = gl, group.equal = "loadings")
f_by <- lapply(levels(d$country), function(g) est(m1, data = d[d$country == g, ]))
names(f_by) <- levels(d$country)

fm <- function(f) {
  v <- lavaan::fitMeasures(f, c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
  round(as.numeric(v), 3)
}
fits <- rbind(
  c(model = "configural, no residual correlations", fm(f_conf0)),
  c(model = "configural, with residual correlations", fm(f_conf)),
  c(model = paste("configural,", levels(d$country)[1]), fm(f_by[[1]])),
  c(model = paste("configural,", levels(d$country)[2]), fm(f_by[[2]])),
  c(model = "metric (loadings equal)", fm(f_metric)))
colnames(fits)[-1] <- c("chisq_scaled", "df", "cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr")
fits <- as.data.frame(fits)
readr::write_csv(fits, file.path(cfg$tables_dir, "03_measurement_fit.csv"))

lrt <- lavaan::lavTestLRT(f_conf, f_metric)
message("Robust chi-square difference, metric vs configural: ",
        sprintf("chi2(%d) = %.2f, p = %.3f", lrt$`Df diff`[2], lrt$`Chisq diff`[2], lrt$`Pr(>Chisq)`[2]))

# ---- Table A2: standardized loadings from the metric model, by country ---------
ss <- lavaan::standardizedSolution(f_metric)
ss <- ss[ss$op == "=~", ]
grp <- lavaan::lavInspect(f_metric, "group.label")
ss$country <- grp[ss$group]
tabA2 <- ss |>
  dplyr::select(construct = lhs, item = rhs, country, loading = est.std) |>
  tidyr::pivot_wider(names_from = country, values_from = loading) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(grp), ~ round(.x, 2)))
readr::write_csv(tabA2, file.path(cfg$tables_dir, "03_table_A2_loadings.csv"))

message("\nFit of the measurement models (compare with the manuscript):")
print(fits, row.names = FALSE)
message("Manuscript: no residuals chi2(538) = 1784.53, CFI = .901 | configural chi2(528) = 1325.44, CFI = .937 | ",
        "Brazil chi2(264) = 621.19 | U.S. chi2(264) = 702.72 | metric chi2(549) = 1360.42, CFI = .936")
message("\nStandardized loadings (Table A2):"); print(as.data.frame(tabA2), row.names = FALSE)
message("Loading range: ", paste(grp, sapply(tabA2[, grp], function(x) paste(range(x), collapse = " to ")), collapse = " | "))
