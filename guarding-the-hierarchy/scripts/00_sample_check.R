# 00_sample_check.R --------------------------------------------------------
# Not part of run_all.R. Run it by itself to see where the analysis sample size
# comes from and to find which variables bring it to the N used in the papers.
#   source("scripts/00_sample_check.R")
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages()
target <- 1315                                   # the N reported in the papers

num <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))
raw <- haven::read_sav(cfg$data_file)
is_num <- vapply(raw, function(x) is.numeric(x) || inherits(x, "haven_labelled"), logical(1))
nums <- as.data.frame(lapply(raw[is_num], num))
names(nums) <- names(raw)[is_num]

# Scale scores built the same way as in 01_clean.R
scores <- as.data.frame(lapply(cfg$constructs, function(k) {
  make_scale(nums, k$items, cfg$min_valid_items)
}))
names(scores) <- names(cfg$constructs)
base_ok <- stats::complete.cases(scores)
cat("\nRespondents in the file:", nrow(raw), "\n")
cat("Missing on each scale score:\n"); print(colSums(is.na(scores)))
cat("Complete on all", ncol(scores), "scale scores:", sum(base_ok), "\n")

# Each control, added one at a time and all together
ctrl <- cfg$controls
cat("\nComplete cases when each control is added to the scale scores:\n")
one <- vapply(ctrl, function(v) sum(base_ok & !is.na(nums[[v]])), numeric(1)); print(one)
cat("All controls together:", sum(base_ok & stats::complete.cases(nums[, ctrl, drop = FALSE])), "\n")

# Gender categories, by country
if (cfg$country_var %in% names(nums)) {
  cat("\nGender by country (rows = gender value):\n")
  print(table(nums[["Gender"]], factor(nums[[cfg$country_var]], levels = unname(cfg$country_codes),
                                       labels = names(cfg$country_codes)), useNA = "ifany"))
}
for (v in intersect(c("qual", "filter_$", "ManCheck", "Racism_valid"), names(nums))) {
  cat("\nValues of", v, "(count):\n"); print(table(nums[[v]], useNA = "ifany"))
}

# Which single extra column brings the complete-case N to exactly the target?
extra <- vapply(names(nums), function(v) sum(base_ok & !is.na(nums[[v]])), numeric(1))
hit <- extra[extra == target]
cat("\nColumns that give exactly N =", target, "when added to the scale scores:\n")
if (length(hit)) print(names(hit)) else cat("  none\n")
cat("\nIf no control gives", target, "look at the list above for a column in the paper's model.\n")
