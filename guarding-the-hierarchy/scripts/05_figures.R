# 05_figures.R -------------------------------------------------------------
# Two figures from the tables made earlier:
#   1. Indirect effects (with 95% intervals) by country, for each outcome.
#   2. Correlation heat maps by country.
# -------------------------------------------------------------------------
source("config.R"); source("R/helpers.R"); load_packages(); ensure_dirs(cfg)
labels <- vapply(cfg$constructs, `[[`, character(1), "label")
cols <- country_colors(cfg)

# ---- 1. Indirect effects -------------------------------------------------------
eff <- readr::read_csv(file.path(cfg$tables_dir, "04_indirect_effects.csv"), show_col_types = FALSE) |>
  dplyr::filter(spec == "main", analysis == "multigroup", grepl("^ind_", label))

sfx <- group_suffix(names(cfg$country_codes))
eff$country <- names(cfg$country_codes)[match(sub(".*_", "", eff$label), sfx)]
eff$outcome <- NA_character_; eff$mediator <- NA_character_
for (o in cfg$outcomes) for (m in cfg$mediators) {
  hit <- startsWith(eff$label, paste0("ind_", m, "_", o, "_"))
  eff$outcome[hit] <- labels[[o]]; eff$mediator[hit] <- labels[[m]]
}

p1 <- ggplot2::ggplot(eff, ggplot2::aes(x = est, y = mediator, color = country)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = ci.lower, xmax = ci.upper),
                           position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::facet_wrap(~ outcome, ncol = 1) +
  ggplot2::scale_color_manual(values = cols, name = NULL) +
  ggplot2::labs(x = paste0("Indirect effect of ", tolower(labels[[cfg$predictor]]), " (95% interval)"), y = NULL,
                title = "Indirect effects through misogyny and authoritarianism") +
  theme_paper()
ggplot2::ggsave(file.path(cfg$figures_dir, "fig1_indirect_effects.png"), p1, width = 7, height = 5, dpi = 300)

# ---- 2. Correlation heat maps --------------------------------------------------
cor_tab <- readr::read_csv(file.path(cfg$tables_dir, "02_correlations_long.csv"), show_col_types = FALSE) |>
  dplyr::filter(var1 %in% names(labels), var2 %in% names(labels)) |>
  dplyr::mutate(var1 = unname(labels[var1]), var2 = unname(labels[var2]))
p2 <- ggplot2::ggplot(cor_tab, ggplot2::aes(var1, var2, fill = r)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 3) +
  ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", limits = c(-1, 1), name = "r") +
  ggplot2::facet_wrap(~ country) +
  ggplot2::labs(x = NULL, y = NULL, title = "Correlations among the main variables") +
  theme_paper() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
ggplot2::ggsave(file.path(cfg$figures_dir, "fig2_correlations.png"), p2, width = 10, height = 5, dpi = 300)
message("Figures saved in ", cfg$figures_dir)
