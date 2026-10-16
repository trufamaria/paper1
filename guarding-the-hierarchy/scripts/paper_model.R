# R/paper_model.R ----------------------------------------------------------
# Model syntax for the manuscript's latent-variable models.
#   Measurement: four latent factors (need for chaos, misogyny, authoritarianism,
#   hostile content sharing). Willingness to engage in political violence is one
#   observed item. Five within-construct residual correlations (config.R).
#   Structure: need for chaos -> (misogyny, authoritarianism) -> (sharing, political violence),
#   with (partial) or without (full) direct paths from need for chaos to the outcomes.
#   Residual covariances: between the two mediators and between the two outcomes.
# Groups are estimated with their own structural paths. Loadings are equal across groups.
# -------------------------------------------------------------------------

# Data with the countries in the order of the paper (Brazil first, so differences are Brazil - U.S.)
read_paper_data <- function(cfg) {
  d <- read_clean(cfg)
  d$country <- factor(as.character(d$country), levels = cfg$group_order)
  d
}

# Name used inside lavaan for a construct: latent factor if it has several items, else the item itself
node <- function(cfg, v) {
  k <- cfg$constructs[[v]]
  if (length(k$items) > 1) paste0("L_", v) else k$items[1]
}
latent_constructs <- function(cfg) names(cfg$constructs)[vapply(cfg$constructs, function(k) length(k$items) > 1, logical(1))]

measurement_syntax <- function(cfg, residuals = TRUE) {
  lv <- latent_constructs(cfg)
  lines <- vapply(lv, function(v) paste0(node(cfg, v), " =~ ", paste(cfg$constructs[[v]]$items, collapse = " + ")), character(1))
  if (residuals) lines <- c(lines, cfg$residual_correlations)
  paste(lines, collapse = "\n")
}

# Structural part. Paths are labelled per group: a1_brazil, a1_us, and so on.
#   a1: predictor -> mediator 1, a2: predictor -> mediator 2
#   b11: mediator 1 -> outcome 1, b12: mediator 2 -> outcome 1, b21, b22: same for outcome 2
#   c1, c2: direct paths from the predictor to outcome 1 and outcome 2 (partial mediation only)
structural_syntax <- function(cfg, groups, partial = TRUE) {
  sfx <- group_suffix(groups)
  lab <- function(base) paste0("c(", paste(paste0(base, "_", sfx), collapse = ", "), ")*")
  X <- node(cfg, cfg$predictor); M <- vapply(cfg$mediators, function(v) node(cfg, v), character(1))
  Y <- vapply(cfg$outcomes, function(v) node(cfg, v), character(1))
  cov <- if (length(cfg$covariates)) paste0(" + ", paste(cfg$covariates, collapse = " + ")) else ""
  L <- character(0)
  for (i in 1:2) L <- c(L, sprintf("%s ~ %s%s%s", M[i], lab(paste0("a", i)), X, cov))
  for (j in 1:2) {
    rhs <- c(if (partial) paste0(lab(paste0("c", j)), X),
             paste0(lab(paste0("b", j, "1")), M[1]), paste0(lab(paste0("b", j, "2")), M[2]))
    L <- c(L, sprintf("%s ~ %s%s", Y[j], paste(rhs, collapse = " + "), cov))
  }
  L <- c(L, sprintf("%s ~~ %s", M[1], M[2]), sprintf("%s ~~ %s", Y[1], Y[2]))
  # Indirect effects: outcome j through mediator i = a_i * b_ji, per group
  for (s in sfx) {
    for (j in 1:2) {
      L <- c(L,
             sprintf("ind_m1_y%d_%s := a1_%s * b%d1_%s", j, s, s, j, s),
             sprintf("ind_m2_y%d_%s := a2_%s * b%d2_%s", j, s, s, j, s),
             sprintf("ind_tot_y%d_%s := ind_m1_y%d_%s + ind_m2_y%d_%s", j, s, j, s, j, s))
    }
  }
  # Differences: first group minus second group (Brazil - U.S.)
  s1 <- sfx[1]; s2 <- sfx[2]
  for (j in 1:2) for (w in c("m1", "m2", "tot"))
    L <- c(L, sprintf("diff_%s_y%d := ind_%s_y%d_%s - ind_%s_y%d_%s", w, j, w, j, s1, w, j, s2))
  paste(L, collapse = "\n")
}

paper_model <- function(cfg, groups, partial = TRUE) {
  paste(measurement_syntax(cfg, residuals = TRUE), structural_syntax(cfg, groups, partial), sep = "\n")
}

# Fit measures used in the manuscript
paper_fit_measures <- function(fit) {
  nm <- c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled",
          "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")
  v <- lavaan::fitMeasures(fit, nm)
  stats::setNames(round(as.numeric(v), 3), nm)
}
