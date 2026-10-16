# config.R ---------------------------------------------------------------
# The only file you need to edit. Every script reads its settings from here.
# Lines marked  <-- EDIT  hold placeholders. Replace them with the column
# names in your data file.
# -------------------------------------------------------------------------

cfg <- list(

  # ---- Files --------------------------------------------------------------
  data_file    = "data/Merged_BR_US_Updated.sav",   # <-- EDIT if the file name differs
  derived_file = "data/analysis_data.rds",          # made by 01_clean.R (stays private)
  tables_dir   = "output/tables",
  figures_dir  = "output/figures",

  # ---- Country ------------------------------------------------------------
  country_var   = "Is_Br",                          # column that tells U.S. from Brazil
  country_codes = c(US = 1, Brazil = 0),            # raw values in Is_Br: 1 = U.S., 0 = Brazil
  # The order above matters. The first country is "group 1" in the
  # multi-group models, and differences are computed as first minus second.

  # ---- Optional case filter ---------------------------------------------------
  # Keep only rows where this R expression is TRUE, for example "qual == 1".
  # NULL = keep every respondent. Use backticks for odd names, such as "`filter_$` == 1".
  keep_if = "Gender %in% c(1, 2) & nchar(as.character(Age)) <= 3",   # men and women only; drops implausible age entries (same rule as the dissertation scripts) -> N = 1315

  # ---- Optional survey weight --------------------------------------------
  # NULL = unweighted models with bootstrapped confidence intervals.
  # A column name = weighted models (robust ML, delta-method intervals).
  weight_var = NULL,                                # <-- EDIT, for example "weight"

  # ---- Constructs ---------------------------------------------------------
  # items   : the survey columns that make up the scale. One name = a single
  #           item or an existing score (used as it is).
  # reverse : items to reverse-code before averaging.
  # range   : lowest and highest possible answer. NULL = read it from the data.
  # index   : (optional) the score already in the file. 01_clean.R compares
  #           it with the new score. A low correlation usually means a
  #           reverse-coded item is missing from `reverse`.
  constructs = list(
    nfc = list(label = "Need for chaos",
               items = paste0("Chaos_", 1:7),
               reverse = character(0),              # <-- CHECK
               range = NULL, index = "Chaos_i"),
    misogyny = list(label = "Misogyny",
               items = paste0("Misg", 1:10),
               reverse = character(0),              # <-- CHECK
               range = NULL, index = "Misg_i"),
    authoritarianism = list(label = "Authoritarianism",
               items = paste0("AT", 1:4),
               reverse = character(0),              # <-- CHECK
               range = NULL, index = "AT_i"),
    destabilizing_sharing = list(label = "Destabilizing content sharing",
               items = paste0("Share", 1:4),
               reverse = character(0),              # <-- CHECK
               range = NULL, index = "Share_i"),
    political_violence = list(label = "Willingness to engage in political violence",
               items = "WEPV",                      # <-- CHECK: one column, used as it is
               reverse = character(0),
               range = NULL, index = NULL)
  ),

  # ---- Model roles (names must match the construct names above) ----------
  predictor = "nfc",
  mediators = c("misogyny", "authoritarianism"),
  outcomes  = c("destabilizing_sharing", "political_violence"),

  # ---- Control variables (numeric columns; Age is entered as a number) ---
  controls = c("Age", "Gender"),
  # Controls listed here are turned into 0/1 dummies by 01_clean.R. The
  # reference category is set in reference_levels below. A category with fewer than min_dummy_n
  # people in either country is too small to model: those people get missing
  # values and drop out of the models (01_clean.R prints how many).
  categorical_controls = "Gender",
  reference_levels = c(Gender = 1),   # reference category for each one (Gender: 1 = men). Remove a line to use the most common category.
  min_dummy_n = 20,
  pol_orientation_var = "Pol_Tend",                 # 1 = extreme left ... 5 = extreme right

  # ---- Estimation ---------------------------------------------------------
  n_boot            = 2000,    # bootstrap draws (use 200 for a quick test run)
  n_cores           = max(1, parallel::detectCores() - 1),  # bootstrap runs on several cores (Mac/Linux)
  specs_to_run      = c("main", "no_controls"),  # use "main" alone for a first test
  missing           = "fiml",  # "fiml" keeps everyone with a valid country and uses all available answers (N = 1315). "listwise" drops anyone with a missing score.
  seed              = 20261015,
  min_valid_items   = 0.80,    # share of scale items a person must answer to get a score
  run_measurement   = TRUE     # CFA and invariance tests (needs 3+ items per scale)
)
