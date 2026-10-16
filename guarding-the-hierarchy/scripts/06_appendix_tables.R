# 06_appendix_tables.R -----------------------------------------------------
# Builds the appendix tables from the clean analysis file.
#   Table A1: alpha, mean, SD, and n by country (written to 06_table_A1.csv)
#   Table A2: correlations among the five study variables, with the U.S. below
#             the diagonal and Brazil above it (written to 06_table_A2.csv)
# Correlations use all available pairs. * p < .05, ** p < .01.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages(); ensure_dirs(cfg)
dat <- read_clean(cfg)
vars <- names(cfg$constructs)
labs <- vapply(cfg$constructs, `[[`, character(1), "label")
us <- levels(dat$country)[1]; br <- levels(dat$country)[2]

# ---- Table A1 ---------------------------------------------------------------
fmt <- function(x, d = 2) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = d))
a1 <- purrr::map_dfr(vars, function(v) {
  k <- cfg$constructs[[v]]
  one <- function(g) {
    d <- dat[dat$country == g, ]
    al <- if (length(k$items) > 1)
      fmt(suppressWarnings(psych::alpha(d[, k$items], check.keys = FALSE, warnings = FALSE))$total$raw_alpha) else "-"
    c(al, fmt(mean(d[[v]], na.rm = TRUE)), fmt(sd(d[[v]], na.rm = TRUE)))
  }
  b <- one(br); u <- one(us)
  tibble::tibble(Construct = k$label, Items = length(k$items),
                 `Brazil alpha` = b[1], `Brazil M` = b[2], `Brazil SD` = b[3],
                 `US alpha` = u[1], `US M` = u[2], `US SD` = u[3])
})
a1 <- dplyr::bind_rows(a1, tibble::tibble(Construct = "Sample (n)", Items = NA,
        `Brazil alpha` = as.character(sum(dat$country == br)), `US alpha` = as.character(sum(dat$country == us))))
readr::write_csv(a1, file.path(cfg$tables_dir, "06_table_A1.csv"))

# ---- Table A2 ---------------------------------------------------------------
stars <- function(p) ifelse(p < .01, "**", ifelse(p < .05, "*", ""))
cm <- function(g) {
  d <- dat[dat$country == g, vars]
  ct <- psych::corr.test(d, use = "pairwise", adjust = "none")
  list(r = ct$r, p = ct$p)
}
cu <- cm(us); cb <- cm(br)
m <- matrix("", length(vars), length(vars), dimnames = list(paste0(seq_along(vars), ". ", labs), seq_along(vars)))
for (i in seq_along(vars)) for (j in seq_along(vars)) {
  if (i == j) m[i, j] <- "-"
  else if (i > j) m[i, j] <- paste0(fmt(cu$r[i, j]), stars(cu$p[i, j]))   # U.S. below
  else            m[i, j] <- paste0(fmt(cb$r[i, j]), stars(cb$p[i, j]))   # Brazil above
}
a2 <- tibble::as_tibble(m, rownames = "Variable")
readr::write_csv(a2, file.path(cfg$tables_dir, "06_table_A2.csv"))
message("Table A1 and A2 written to ", cfg$tables_dir, " (06_table_A1.csv, 06_table_A2.csv).")
message("Note for A2: U.S. correlations below the diagonal, Brazil above. * p < .05, ** p < .01.")
print(a1); print(a2)
