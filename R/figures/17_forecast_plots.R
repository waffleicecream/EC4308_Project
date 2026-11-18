# Predicted vs actual h-month log equity premium over the test period, at
# non-overlapping origins (every h months, as in the backtest), with the
# historical-mean forecast. One figure per model, one panel per horizon.
#
# Outputs: output/figures/forecasts/<model>_ext.png

source("R/00_config.R")
source("R/functions/plot_style.R")

MODELS  <- c("ensemble_mean", "rf", "bag", "xgb", "lasso_bic", "rlasso",
             "post_lasso", "pca_lasso", "pcr_rlasso", "ridge", "kitchen_sink")
out_dir <- fig_dir("forecasts")

fc <- bind_rows(lapply(file.path(FCST_DIR, paste0(c("hm", MODELS), ".rds")), readRDS)) |>
  group_by(model, set, h) |>
  arrange(date, .by_group = TRUE) |>
  filter((row_number() - 1) %% h == 0) |>
  ungroup()

hm <- fc |> filter(model == "hm")
for (m in MODELS) for (s in unique(fc$set[fc$model == m])) {
  mod <- fc |> filter(model == m, set == s)
  dp  <- bind_rows(
    mod |> transmute(h, date, series = "Actual",          value = y),
    hm  |> transmute(h, date, series = "Historical mean", value = yhat),
    mod |> transmute(h, date, series = m,                 value = yhat)
  ) |>
    mutate(value  = 100 * value,
           series = factor(series, levels = c("Actual", "Historical mean", m)),
           hl     = h_factor(h))

  g <- ggplot(dp, aes(date, value, colour = series, linewidth = series)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
    geom_line() +
    geom_point(data = filter(dp, h != 1), size = 1.2) +
    facet_wrap(~ hl, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = setNames(c(INK, GREY, BLUE), levels(dp$series))) +
    scale_linewidth_manual(values = setNames(c(0.4, 0.6, 0.8), levels(dp$series))) +
    labs(title = sprintf("Predicted vs actual equity premium - %s, %s set", m, s),
         subtitle = paste0("Test period ", format(TEST_START, "%Y"), "-", format(TEST_END, "%Y"),
                           ", non-overlapping origins (every h months). ",
                           "Value = log equity premium over months t+1..t+h."),
         x = "Forecast origin t", y = "Log equity premium (%)",
         colour = NULL, linewidth = NULL)

  ggsave(file.path(out_dir, sprintf("%s_%s.png", m, s)), g, width = 9, height = 8, dpi = 150)
}
cat("Saved forecast figures to", out_dir, "\n")
