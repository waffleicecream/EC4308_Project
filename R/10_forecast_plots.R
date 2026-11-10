# =============================================================================
# 10_forecast_plots.R  -  predicted vs actual equity premium over the test set
#
# One figure per model (rf, bag, rf_lag, ensemble_mean) x predictor set
# (gw, ext), with one panel per horizon (h = 1, 3, 12). Each panel shows,
# at non-overlapping origins (every h months, the same decision dates as
# 04_trading.R):
#   - actual h-month log equity premium (sum over months t+1..t+h)
#   - historical-mean benchmark forecast (hm)
#   - the model's forecast
#
# Outputs: output/figures/forecasts/<model>_<set>.png
# =============================================================================

source("R/00_config.R")
suppressPackageStartupMessages(library(ggplot2))

MODELS <- c("rf", "bag", "rf_lag", "ensemble_mean")
out_dir <- file.path(FIG_DIR, "forecasts")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fc <- bind_rows(lapply(file.path(FCST_DIR, paste0(c("hm", MODELS), ".rds")), readRDS))

# Non-overlapping origins per horizon (as in 04_trading.R)
fc <- fc |>
  group_by(model, set, h) |>
  arrange(date, .by_group = TRUE) |>
  filter((row_number() - 1) %% h == 0) |>
  ungroup()

# ---- Style (same as 05_xgb_interpret.R) ----------------------------------------
BLUE  <- "#2a78d6"   # model forecast
GREY  <- "#8a8984"   # historical mean
INK   <- "#0b0b0b"; INK2 <- "#52514e"; SURF <- "#fcfcfb"
theme_set(theme_minimal(base_size = 11) +
  theme(plot.background  = element_rect(fill = SURF, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        text = element_text(colour = INK), axis.text = element_text(colour = INK2),
        plot.subtitle = element_text(colour = INK2, size = 9.5),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.justification = "left",
        plot.title.position = "plot"))
h_lab <- function(h) paste0("h = ", h, (if (h == 1) " month" else " months"))

# ---- Plots ------------------------------------------------------------------------
for (m in MODELS) for (s in c("gw", "ext")) {
  mod <- fc |> filter(model == m, set == s)
  hm  <- fc |> filter(model == "hm")
  dp  <- bind_rows(
    mod |> transmute(h, date, series = "Actual",          value = y),
    hm  |> transmute(h, date, series = "Historical mean", value = yhat),
    mod |> transmute(h, date, series = m,                 value = yhat)
  ) |>
    mutate(value  = 100 * value,
           series = factor(series, levels = c("Actual", "Historical mean", m)),
           hl     = factor(sapply(h, h_lab), levels = sapply(HORIZONS, h_lab)))

  g <- ggplot(dp, aes(date, value, colour = series, linewidth = series)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
    geom_line() +
    geom_point(data = filter(dp, h != 1), size = 1.2) +
    facet_wrap(~ hl, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = setNames(c(INK, GREY, BLUE), levels(dp$series))) +
    scale_linewidth_manual(values = setNames(c(0.4, 0.6, 0.8), levels(dp$series))) +
    labs(title = sprintf("Predicted vs actual equity premium - %s, %s set", m, s),
         subtitle = paste("Test set 2016-2025, non-overlapping origins (every h months).",
                          "Value = log equity premium over months t+1..t+h."),
         x = "Forecast origin t", y = "Log equity premium (%)",
         colour = NULL, linewidth = NULL)

  ggsave(file.path(out_dir, sprintf("%s_%s.png", m, s)), g,
         width = 9, height = 8, dpi = 150)
}
cat("Saved", length(MODELS) * 2, "figures to", out_dir, "\n")
