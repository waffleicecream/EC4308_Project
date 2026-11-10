# =============================================================================
# 12_performance_plots.R  -  target over time, and cumulative trading performance
#
# 1. Target. Log equity premium at each horizon (y1, y3, y12): the sum of
#    monthly log premia over t+1..t+h, dated at the end of that window.
#    Full sample, with the 2016-2025 test window shaded.
#
# 2. Cumulative performance. Compounded return of the S&P 500 / T-bill
#    switch in 04_trading.R, for every saved model, over the test set.
#    Buy-and-hold and always-T-bills are drawn on top. The historical-mean
#    strategy is not drawn separately: its forecast stays positive, so its
#    wealth path is the buy-and-hold path. Perfect foresight is left out
#    (it is an upper bound, and its scale hides the models).
#
# Outputs: output/figures/target_equity_premium.png
#          output/figures/cum_performance.png
# Run 01_data_prep.R and 04_trading.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
suppressPackageStartupMessages(library(ggplot2))

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Style (same as 10_forecast_plots.R) -----------------------------------
BLUE  <- "#2a78d6"
INK   <- "#0b0b0b"; INK2 <- "#52514e"; SURF <- "#fcfcfb"; GREY <- "#8a8984"
theme_set(theme_minimal(base_size = 11) +
  theme(plot.background  = element_rect(fill = SURF, colour = NA),
        panel.background = element_rect(fill = SURF, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        text = element_text(colour = INK), axis.text = element_text(colour = INK2),
        plot.subtitle = element_text(colour = INK2, size = 9.5),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.justification = "left",
        plot.title.position = "plot"))
h_lab <- function(h) paste0("h = ", h, (if (h == 1) " month" else " months"))
h_factor <- function(h) factor(sapply(h, h_lab), levels = sapply(HORIZONS, h_lab))

# Month in which the h-month target is fully realised.
realised_end <- function(origin, h) month_end(origin %m+% months(h))

# ---- 1. Target across time --------------------------------------------------
d <- readRDS(PROC_FILE)$data
target <- bind_rows(lapply(HORIZONS, function(hh) {
  tibble(h     = hh,
         date  = realised_end(d$date, hh),
         value = 100 * d[[target_col(hh)]])
})) %>%
  filter(!is.na(value)) %>%
  mutate(hl = h_factor(h))
stopifnot(!anyDuplicated(target[, c("h", "date")]))

g_target <- ggplot(target, aes(date, value)) +
  annotate("rect", xmin = TEST_START, xmax = TEST_END, ymin = -Inf, ymax = Inf,
           fill = BLUE, alpha = 0.10) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line(colour = INK, linewidth = 0.35) +
  facet_wrap(~ hl, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Log equity premium across time",
       subtitle = paste("Sum of monthly log equity premia over the next h months,",
                        "dated when that window ends.",
                        "Shaded band is the test period, 2016-2025."),
       x = NULL, y = "Log equity premium (%)")

ggsave(file.path(FIG_DIR, "target_equity_premium.png"), g_target,
       width = 10, height = 8, dpi = 150)

# ---- 2. Cumulative performance ----------------------------------------------
paths <- readRDS(file.path(TAB_DIR, "trading_paths.rds"))

present <- setdiff(unique(paths$model),
                   c("perfect_foresight", "buy_hold", "tbill", "hm",
                     "rf_lag", "bag_lag"))
preferred <- c("ensemble_mean", "rf", "bag", "xgb",
               "lasso_bic", "rlasso", "post_lasso", "pca_lasso", "pcr_rlasso",
               "ridge")
MODELS <- c(intersect(preferred, present), sort(setdiff(present, preferred)))

# Draw references last so the dashed buy-and-hold line stays visible where a
# model's path sits on top of it. Legend order is set separately below.
series_levels <- c(MODELS, "buy_hold", "tbill")
series_breaks <- c("buy_hold", "tbill", MODELS)
series_labels <- c(buy_hold = "buy & hold", tbill = "T-bills",
                   setNames(MODELS, MODELS))

model_cols <- c(
  ensemble_mean = "#c0392b",
  rf            = "#2a78d6",
  bag           = "#d35400",
  xgb           = "#8e44ad",
  lasso_bic     = "#1e8449",
  rlasso        = "#27ae60",
  post_lasso    = "#0e6655",
  pca_lasso     = "#b7950b",
  pcr_rlasso    = "#7d6608",
  ridge         = "#5d6d7e",
  buy_hold      = INK,
  tbill         = GREY
)
extra <- setdiff(MODELS, names(model_cols))
if (length(extra)) {
  model_cols <- c(model_cols,
                  setNames(scales::hue_pal()(length(extra)), extra))
}

mods <- paths %>%
  filter(model %in% MODELS, set %in% c("gw", "ext")) %>%
  transmute(set, h, date, model, cum = 100 * (wealth / W0 - 1))
refs <- paths %>%
  filter(model %in% c("buy_hold", "tbill")) %>%
  select(h, date, model, wealth) %>%
  crossing(set = c("gw", "ext")) %>%
  transmute(set, h, date, model, cum = 100 * (wealth / W0 - 1))

perf <- bind_rows(mods, refs) %>%
  mutate(series = factor(model, levels = series_levels),
         hl     = h_factor(h),
         set    = factor(set, levels = c("gw", "ext")))

# Start every path at 0 on the eve of the test period.
perf <- bind_rows(
  perf %>% distinct(set, h, hl, series) %>%
    mutate(date = FIRST_ORIGIN, cum = 0),
  perf
)

line_w <- c(setNames(rep(0.6, length(MODELS)), MODELS),
            buy_hold = 0.8, tbill = 0.7)
line_t <- c(setNames(rep("solid", length(MODELS)), MODELS),
            buy_hold = "dashed", tbill = "dotted")

g_perf <- ggplot(perf, aes(date, cum, colour = series,
                           linewidth = series, linetype = series)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line() +
  facet_grid(hl ~ set, labeller = labeller(
    set = c(gw = "gw set", ext = "ext set"))) +
  scale_colour_manual(values = model_cols, breaks = series_breaks,
                      labels = series_labels) +
  scale_linewidth_manual(values = line_w, breaks = series_breaks,
                         labels = series_labels) +
  scale_linetype_manual(values = line_t, breaks = series_breaks,
                        labels = series_labels) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Cumulative trading performance",
       subtitle = paste("Test set 2016-2025. Long the S&P 500 when the forecast is",
                        "positive, otherwise T-bills. Cost 0.2% per switch.",
                        "Dashed line is buy-and-hold, which matches the historical mean."),
       x = NULL, y = "Cumulative return (%)",
       colour = NULL, linewidth = NULL, linetype = NULL) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        legend.text = element_text(size = 8)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 0.9)))

ggsave(file.path(FIG_DIR, "cum_performance.png"), g_perf,
       width = 11, height = 9, dpi = 150)

cat("Saved target_equity_premium.png and cum_performance.png to", FIG_DIR, "\n")
