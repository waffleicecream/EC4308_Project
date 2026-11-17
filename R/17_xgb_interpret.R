# =============================================================================
# 17_xgb_interpret.R  -  relative importance and partial dependence plots
#
# Uses the models refitted at each annual tuning date during the POOS run
# (annual tuning dates, 1995-12 through 2024-12), logged by 03_xgb.R:
#   output/tables/xgb_importance.csv   gain per variable at each date
#   output/tables/xgb_pdp.csv          partial dependence curves at each date
#
# Relative importance (lecture slides 38-39), per set x horizon:
#   1. gain_l(date) = share of total squared-error improvement from splits on
#      variable l in that date's model (= squared relevance I_l^2)
#   2. average over the annual tuning dates
#   3. take the square root, scale so the most important variable = 100
#
# Partial dependence (lecture slide 41), per set x horizon, top 6 variables:
#   each date's curve f_S(v) = mean over training rows of f(v, x_iC);
#   plotted as thin lines (one per date) plus their average (thick line)
#
# Outputs: output/figures/xgb_importance_<set>.png
#          output/figures/xgb_pdp_<set>_h<h>.png
#          output/tables/xgb_importance_summary.csv
# =============================================================================

source("R/00_config.R")
suppressPackageStartupMessages(library(ggplot2))

imp <- read.csv(file.path(TAB_DIR, "xgb_importance.csv"))
pdp <- read.csv(file.path(TAB_DIR, "xgb_pdp.csv"))
d   <- readRDS(PROC_FILE)$data
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Style --------------------------------------------------------------------
BLUE  <- "#2a78d6"   # main series
LIGHT <- "#9ec5f4"   # individual tuning dates (same hue, lighter)
INK   <- "#0b0b0b"; INK2 <- "#52514e"; SURF <- "#fcfcfb"
theme_set(theme_minimal(base_size = 11) +
  theme(plot.background  = element_rect(fill = SURF, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        text = element_text(colour = INK), axis.text = element_text(colour = INK2),
        plot.subtitle = element_text(colour = INK2, size = 9.5),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot"))
h_lab <- function(h) paste0("h = ", h, (if (h == 1) " month" else " months"))

# ---- 1. Relative importance -----------------------------------------------------
summ <- imp |>
  group_by(set, h, feature) |>
  summarise(mean_gain = mean(gain), n_dates = n(), .groups = "drop_last") |>
  mutate(rel_importance = 100 * sqrt(mean_gain) / max(sqrt(mean_gain))) |>
  arrange(set, h, desc(rel_importance)) |>
  mutate(rank = row_number()) |>
  ungroup()
write.csv(summ, file.path(TAB_DIR, "xgb_importance_summary.csv"), row.names = FALSE)

for (s in unique(summ$set)) {
  ds <- summ |> filter(set == s, rank <= 15) |>
    mutate(hl  = factor(sapply(h, h_lab), levels = sapply(HORIZONS, h_lab)),
           key = factor(paste(feature, h, sep = "___")))
  ds$key <- reorder(ds$key, ds$rel_importance)      # order bars within panel
  n_tune <- max(ds$n_dates)
  g <- ggplot(ds, aes(rel_importance, key)) +
    geom_col(fill = BLUE, width = 0.7) +
    geom_text(aes(label = round(rel_importance)), hjust = -0.25, size = 3, colour = INK2) +
    facet_wrap(~ hl, scales = "free_y") +
    scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
    scale_x_continuous(limits = c(0, 112), breaks = c(0, 50, 100), expand = c(0, 0)) +
    labs(title = sprintf("XGBoost relative importance - %s predictor set", s),
         subtitle = paste0("sqrt(average gain over the ", n_tune,
                           " annual tuning dates), most ",
                           "important variable = 100",
                           if (s == "ext") " | top 15 shown" else ""),
         x = "Relative importance", y = NULL)
  ggsave(file.path(FIG_DIR, sprintf("xgb_importance_%s.png", s)), g,
         width = 10, height = if (s == "ext") 5.5 else 5, dpi = 150)
}

# ---- 2. Partial dependence --------------------------------------------------------
for (s in unique(summ$set)) for (hh in HORIZONS) {
  top <- summ |> filter(set == s, h == hh, rank <= 6) |> pull(feature)
  dp  <- pdp |> filter(set == s, h == hh, feature %in% top) |>
    mutate(feature = factor(feature, levels = top), yhat = 100 * yhat)
  avg <- dp |> group_by(feature, x) |> summarise(yhat = mean(yhat), .groups = "drop")
  # data distribution: deciles of each variable over the sample (rug ticks)
  rug <- bind_rows(lapply(top, \(v) {
    x <- d[[v]]; x <- x[!is.na(x)]
    data.frame(feature = v, x = if (length(unique(x)) <= 2) unique(x)
                                else quantile(x, seq(0.1, 0.9, 0.1), names = FALSE))
  })) |> mutate(feature = factor(feature, levels = top))

  g <- ggplot(dp, aes(x, yhat)) +
    geom_line(aes(group = origin), colour = LIGHT, linewidth = 0.4) +
    geom_line(data = avg, colour = BLUE, linewidth = 1.1) +
    geom_point(data = avg |> filter(feature %in% top[sapply(top, \(v)
                 length(unique(d[[v]][!is.na(d[[v]])])) <= 2)]),
               colour = BLUE, size = 2.5) +
    geom_rug(data = rug, aes(x = x), inherit.aes = FALSE, sides = "b",
             colour = INK2, length = unit(0.03, "npc")) +
    facet_wrap(~ feature, scales = "free_x", ncol = 3) +
    labs(title = sprintf("XGBoost partial dependence - %s set, %s", s, h_lab(hh)),
         subtitle = paste0("Predicted ", hh, "-month log equity premium (%), other ",
                           "predictors averaged out. Top 6 variables by importance.\n",
                           "Thin lines: model at each of the ", n_distinct(dp$origin),
                           " annual tuning dates; ",
                           "thick line: their average. Ticks: deciles of the variable."),
         x = NULL, y = sprintf("Predicted %d-month premium (%%)", hh))
  ggsave(file.path(FIG_DIR, sprintf("xgb_pdp_%s_h%d.png", s, hh)), g,
         width = 10, height = 6, dpi = 150)
}

cat("Saved figures to", FIG_DIR, "\n")
print(summ |> filter(rank <= 5) |> select(set, h, rank, feature, rel_importance) |>
        mutate(rel_importance = round(rel_importance)), n = Inf)
