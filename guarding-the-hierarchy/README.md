# Guarding the Hierarchy: Analysis Code

R code for the manuscript "Guarding the Hierarchy". The paper tests whether need for chaos leads to destabilizing content sharing and willingness for political violence, through misogyny and authoritarianism, in the United States and Brazil.

**Status:** work in progress. The model and variable names are set in `config.R`. Check them against the manuscript before you use any result.

## What the code does

| Step | Script | Output (in `output/`) |
|------|--------|-----------------------|
| 1. Clean | `scripts/01_clean.R` | Reverse-codes items, builds scale scores, reliability (alpha) by country |
| 2. Describe | `scripts/02_descriptives.R` | Means, SDs, skew, country differences (Welch t, Cohen's d), correlations |
| 3. Measure | `scripts/03_measurement.R` | One-factor CFA by country and invariance tests (configural, metric, scalar) |
| 4. Model | `scripts/04_mediation.R` | Parallel mediation: separate by country, multi-group, and tests of country differences |
| 5. Figures | `scripts/05_figures.R` | Indirect-effects plot and correlation heat maps |

## Folder layout

```
config.R          settings and variable names (the only file you edit)
run_all.R         runs all five steps in order
R/helpers.R       shared functions
scripts/          the five steps
data/             put the survey file here (not uploaded to GitHub)
output/tables/    results as CSV files
output/figures/   PNG figures
```

## How to run

1. Put the survey file in `data/`. The default name is `Merged_BR_US_Updated.sav`.
2. Open `config.R` and fill in the lines marked `<-- EDIT`: the country column, the item columns for each scale, reverse-coded items, and the controls.
3. Open `guarding-the-hierarchy.Rproj` in RStudio and run:

```r
source("run_all.R")
```

The first run installs the packages it needs. It needs R 4.1 or newer. For a quick test, set `n_boot = 500` in `config.R`. The final run uses 2000 bootstrap draws and can take a while.

If R says `cannot open the connection`, the working directory is not the project folder. Open the `.Rproj` file, or run `setwd()` to the folder.

If R says `cannot find data/analysis_data.rds`, step 1 has not run yet. Run `source("run_all.R")`, or run `scripts/01_clean.R` first.

If a column name in `config.R` is wrong, the script stops and lists the columns it found.

## Sample size check

To see where the analysis N comes from, run `source("scripts/00_sample_check.R")`. It prints missing values for each scale, the N after each control, the gender table by country, and any column that brings the N to the figure in the papers (set `target` at the top of that script).

## The model

Need for chaos predicts two mediators (misogyny and authoritarianism). The mediators predict two outcomes (destabilizing content sharing and willingness for political violence). The model also has a direct path from need for chaos to each outcome, and controls on every equation. The two mediators are allowed to correlate, and so are the two outcomes.

- **Separate models** are fitted in each country.
- **A multi-group model** gives every path a label for each country. It reports each indirect effect for each country and the difference between countries (first country minus second).
- **An equality test** compares a model with free paths to a model with equal paths across countries.
- **Specifications:** the main model (with controls), a model with no controls, and a model without political orientation.

Unweighted models use bootstrapped percentile intervals. If you set `weight_var` in `config.R`, the models use robust maximum likelihood with delta-method intervals instead.

## Results files

- `01_reliability.csv`, `02_descriptives.csv`, `02_country_differences.csv`, `02_correlations_long.csv`
- `03_cfa_by_country.csv`, `03_measurement_invariance.csv`
- `04_all_parameters.csv`, `04_indirect_effects.csv`, `04_path_equality_tests.csv`, `04_r2_*.csv`
- `fig1_indirect_effects.png`, `fig2_correlations.png`

## Limits

- The scale scores are item averages. Scales with fewer than 80% of items answered are set to missing.
- Models drop respondents with missing values on any model variable. The script prints how many remain.
- Standardized estimates are point estimates. The bootstrapped intervals are for unstandardized effects.
- Descriptives and correlations are unweighted.
- The raw survey data and respondent-level files are not in this repository.

## Author

Marina Petric, Ph.D., Media and Communication, Texas Tech University
[marinapetric.com](https://marinapetric.com)
