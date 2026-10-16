# run_all.R ----------------------------------------------------------------
# Runs the whole analysis, in order. Open the project in RStudio (or set the
# working directory to this folder) and run:  source("run_all.R")
# For a quick test, set n_boot = 500 in config.R first.
# -------------------------------------------------------------------------
if (!file.exists("config.R")) {
  stop("R cannot see config.R. Open guarding-the-hierarchy.Rproj in RStudio, or run setwd() ",
       "to the project folder, then try again. Current folder: ", getwd(), call. = FALSE)
}
if (!file.exists("data/Merged_BR_US_Updated.sav") && !any(grepl("\\.sav$", list.files("data")))) {
  message("Note: no .sav file found in data/. Put the survey file there and set data_file in config.R.")
}

steps <- c("scripts/01_clean.R", "scripts/02_descriptives.R", "scripts/03_measurement.R",
           "scripts/04_mediation.R", "scripts/05_figures.R")
for (s in steps) {
  message("\n>>> ", s)
  source(s, echo = FALSE)
}
dir.create("output", showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
message("\nDone. Tables are in output/tables and figures in output/figures.")
