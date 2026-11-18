# XGBoost relative importance and partial dependence (Lecture 3.2), from the
# models refit at each annual tuning date (logged by 05_xgb.R).
#
# Relative importance: mean gain over the tuning dates, square-rooted, scaled
# so the top variable = 100. Partial dependence: top 6 variables, one thin
# line per tuning date plus their average.
#
# Outputs: output/figures/xgb/xgb_importance_<set>.png, xgb_pdp_<set>_h<h>.png
#          output/tables/xgb/xgb_importance_summary.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/plot_style.R")

imp_path <- file.path(model_tab_dir("xgb"), "xgb_importance.csv")
pdp_path <- file.path(model_tab_dir("xgb"), "xgb_pdp.csv")
if (!file.exists(imp_path) || !file.exists(pdp_path))
  stop("Missing xgb_importance.csv / xgb_pdp.csv. Run R/models/05_xgb.R first.",
       call. = FALSE)

imp <- read.csv(imp_path)
pdp <- read.csv(pdp_path)
d   <- readRDS(PROC_FILE)$data
out_dir <- fig_dir("xgb")
LIGHT <- "#9ec5f4"

summ <- imp |>
  group_by(set, h, feature) |>
  summarise(mean_gain = mean(gain), n_dates = n(), .groups = "drop_last") |>
  mutate(rel_importance = 100 * sqrt(mean_gain) / max(sqrt(mean_gain))) |>
  arrange(set, h, desc(rel_importance)) |>
  mutate(rank = row_number()) |>
  ungroup()
write.csv(summ, file.path(model_tab_dir("xgb"), "xgb_importance_summary.csv"), row.names = FALSE)
n_dates <- max(summ$n_dates)

for (s in unique(summ$set)) {
  ds <- summ |> filter(set == s, rank <= 15) |>
    mutate(hl = h_factor(h), key = factor(paste(feature, h, sep = "___")))
  ds$key <- reorder(ds$key, ds$rel_importance)
  g <- ggplot(ds, aes(rel_importance, key)) +
    geom_col(fill = BLUE, width = 0.7) +
    geom_text(aes(label = round(rel_importance)), hjust = -0.25, size = 3, colour = INK2) +
    facet_wrap(~ hl, scales = "free_y") +
    scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
    scale_x_continuous(limits = c(0, 112), breaks = c(0, 50, 100), expand = c(0, 0)) +
    labs(title = sprintf("XGBoost relative importance - %s predictor set", s),
         subtitle = sprintf("sqrt(average gain over the %d annual tuning dates), most important variable = 100 | top 15 shown",
                            n_dates),
         x = "Relative importance", y = NULL)
  ggsave(file.path(out_dir, sprintf("xgb_importance_%s.png", s)), g,
         width = 10, height = 5.5, dpi = 150)
}

is_binary <- function(v) length(unique(d[[v]][!is.na(d[[v]])])) <= 2

for (s in unique(summ$set)) for (hh in HORIZONS) {
  top <- summ |> filter(set == s, h == hh, rank <= 6) |> pull(feature)
  dp  <- pdp |> filter(set == s, h == hh, feature %in% top) |>
    mutate(feature = factor(feature, levels = top), yhat = 100 * yhat)
  avg <- dp |> group_by(feature, x) |> summarise(yhat = mean(yhat), .groups = "drop")
  rug <- bind_rows(lapply(top, \(v) {         # deciles of each variable
    x <- d[[v]]; x <- x[!is.na(x)]
    data.frame(feature = v, x = if (is_binary(v)) unique(x)
                                else quantile(x, seq(0.1, 0.9, 0.1), names = FALSE))
  })) |> mutate(feature = factor(feature, levels = top))

  g <- ggplot(dp, aes(x, yhat)) +
    geom_line(aes(group = origin), colour = LIGHT, linewidth = 0.4) +
    geom_line(data = avg, colour = BLUE, linewidth = 1.1) +
    geom_point(data = avg |> filter(feature %in% top[sapply(top, is_binary)]),
               colour = BLUE, size = 2.5) +
    geom_rug(data = rug, aes(x = x), inherit.aes = FALSE, sides = "b",
             colour = INK2, length = unit(0.03, "npc")) +
    facet_wrap(~ feature, scales = "free_x", ncol = 3) +
    labs(title = sprintf("XGBoost partial dependence - %s set, %s", s, h_lab(hh)),
         subtitle = paste0("Predicted ", hh, "-month log equity premium (%), other ",
                           "predictors averaged out. Top 6 variables by importance.\n",
                           "Thin lines: model at each of the ", n_dates, " annual tuning dates; ",
                           "thick line: their average. Ticks: deciles of the variable."),
         x = NULL, y = sprintf("Predicted %d-month premium (%%)", hh))
  ggsave(file.path(out_dir, sprintf("xgb_pdp_%s_h%d.png", s, hh)), g,
         width = 10, height = 6, dpi = 150)
}

cat("Saved figures to", out_dir, "\n")
print(summ |> filter(rank <= 5) |> select(set, h, rank, feature, rel_importance) |>
        mutate(rel_importance = round(rel_importance)), n = Inf)
