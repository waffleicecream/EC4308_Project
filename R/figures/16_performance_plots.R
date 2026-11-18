# 1. The target over the full sample: realised h-month log equity premium and
#    the historical mean known at each origin, with the test period shaded.
# 2. Cumulative trading performance of every model over the test period,
#    against the historical-mean strategy (which stays in the market, so it
#    matches buy-and-hold) and T-bills; and perfect foresight against the
#    historical mean.
# 3. A table joining the trading results with the forecast evaluation.
#    pct_in_mkt = share of test months in the S&P 500; pct_yhat_pos = share
#    of origins with a positive forecast (they differ for h > 1, where the
#    backtest trades only every h months).
#
# Outputs: output/figures/target_equity_premium.png, cum_performance.png,
#          cum_performance_best.png
#          output/tables/cum_performance_ext.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/trading.R")
source("R/functions/metrics.R")
source("R/functions/plot_style.R")

# Month in which the h-month target is fully realised
realised_end <- function(origin, h) month_end(origin %m+% months(h))

# Mean of the realised targets in rows i-h-W+1 .. i-h, truncated at the start
prevailing_mean <- function(y, h, window) {
  vapply(seq_along(y), function(i) {
    last <- i - h
    if (last < 1) return(NA_real_)
    first <- max(1L, last - window + 1L)
    if (anyNA(y[first:last])) return(NA_real_)
    mean(y[first:last])
  }, numeric(1))
}

# ---- 1. Target across time --------------------------------------------------
d <- readRDS(PROC_FILE)$data
hm_fc <- readRDS(file.path(FCST_DIR, "hm.rds"))
series <- bind_rows(lapply(HORIZONS, function(hh) {
  y    <- d[[target_col(hh)]]
  yhat <- prevailing_mean(y, hh, roll_window(d, hh))
  saved <- hm_fc |> filter(h == hh) |> arrange(date)
  stopifnot(isTRUE(all.equal(yhat[match(saved$date, d$date)], saved$yhat)))

  date <- realised_end(d$date, hh)
  ok <- !is.na(y)
  bind_rows(
    tibble(h = hh, date = date[ok], value = 100 * y[ok],    series = "Realised"),
    tibble(h = hh, date = date[ok], value = 100 * yhat[ok], series = "Historical mean")
  )
})) |>
  filter(!is.na(value)) |>
  mutate(hl = h_factor(h),
         series = factor(series, levels = c("Realised", "Historical mean")))

g_target <- ggplot(series, aes(date, value, colour = series, linewidth = series,
                               alpha = series)) +
  annotate("rect", xmin = TEST_START, xmax = TEST_END, ymin = -Inf, ymax = Inf,
           fill = BLUE, alpha = 0.10) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ hl, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("Realised" = INK, "Historical mean" = BLUE)) +
  scale_linewidth_manual(values = c("Realised" = 0.35, "Historical mean" = 0.7)) +
  scale_alpha_manual(values = c("Realised" = 1, "Historical mean" = 0.4)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Log equity premium across time",
       subtitle = paste("Realised h-month log equity premium, dated when the window ends.",
                        "Historical mean: mean of premia known at that origin.",
                        paste0("Shaded band: test period, ", format(TEST_START, "%Y"),
                               "-", format(TEST_END, "%Y"), ".")),
       x = NULL, y = "Log equity premium (%)",
       colour = NULL, linewidth = NULL, alpha = NULL)

ggsave(file.path(fig_dir(), "target_equity_premium.png"), g_target,
       width = 10, height = 8, dpi = 150)

# ---- 2. Cumulative performance ----------------------------------------------
paths <- readRDS(file.path(TAB_DIR, "trading_paths.rds"))

MODEL_ORDER <- c("ensemble_mean", "rf", "bag", "xgb",
                 "lasso_bic", "rlasso", "post_lasso", "pca_lasso", "pcr_rlasso",
                 "ridge", "kitchen_sink")
present <- setdiff(unique(paths$model), c("perfect_foresight", "buy_hold", "tbill", "hm"))
MODELS  <- c(intersect(MODEL_ORDER, present), sort(setdiff(present, MODEL_ORDER)))

# The baseline is drawn last so it stays visible; the legend lists it first
series_levels <- c(MODELS, "tbill", "hm")
series_breaks <- c("hm", "tbill", MODELS)
series_labels <- c(hm = "historical mean", tbill = "T-bills", setNames(MODELS, MODELS))

model_cols <- c(
  ensemble_mean = "#c0392b", rf = "#2a78d6", bag = "#d35400", xgb = "#8e44ad",
  lasso_bic = "#1e8449", rlasso = "#27ae60", post_lasso = "#0e6655",
  pca_lasso = "#b7950b", pcr_rlasso = "#7d6608", ridge = "#5d6d7e",
  kitchen_sink = "#1a5276", hm = INK, tbill = GREY, perfect_foresight = "#1a1a1a"
)
extra <- setdiff(MODELS, names(model_cols))
if (length(extra))
  model_cols <- c(model_cols, setNames(scales::hue_pal()(length(extra)), extra))

to_cum <- function(p) p |> transmute(h, date, model, cum = 100 * (wealth / W0 - 1))

# Start every path at 0 on the eve of the test period
add_zero <- function(p) {
  bind_rows(p |> distinct(h, hl, series) |> mutate(date = FIRST_ORIGIN, cum = 0), p)
}

perf <- bind_rows(
  paths |> filter(model %in% MODELS, set == "ext") |> to_cum(),
  paths |> filter(model %in% c("hm", "tbill")) |> to_cum()
) |>
  mutate(series = factor(model, levels = series_levels), hl = h_factor(h)) |>
  add_zero()

line_w <- c(setNames(rep(0.6, length(MODELS)), MODELS), hm = 0.8, tbill = 0.7)
line_t <- c(setNames(rep("solid", length(MODELS)), MODELS), hm = "dashed", tbill = "dotted")

g_perf <- ggplot(perf, aes(date, cum, colour = series, linewidth = series, linetype = series)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ hl, ncol = 1) +
  scale_colour_manual(values = model_cols, breaks = series_breaks, labels = series_labels) +
  scale_linewidth_manual(values = line_w, breaks = series_breaks, labels = series_labels) +
  scale_linetype_manual(values = line_t, breaks = series_breaks, labels = series_labels) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Cumulative trading performance",
       subtitle = paste(paste0("Ext set, ", format(TEST_START, "%Y"), "-",
                               format(TEST_END, "%Y"), ","),
                        "against the historical-mean strategy. Dotted line: T-bills."),
       x = NULL, y = "Cumulative return (%)",
       colour = NULL, linewidth = NULL, linetype = NULL) +
  theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"),
        legend.text = element_text(size = 8)) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE,
                               override.aes = list(linewidth = 0.9)))

best_labels <- c(hm = "historical mean", perfect_foresight = "theoretical best")
best <- paths |>
  filter(model %in% names(best_labels)) |>
  to_cum() |>
  mutate(series = factor(model, levels = names(best_labels)), hl = h_factor(h)) |>
  add_zero()

g_perf_best <- ggplot(best, aes(date, cum, colour = series, linewidth = series, linetype = series)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ hl, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = model_cols, labels = best_labels) +
  scale_linewidth_manual(values = c(hm = 0.8, perfect_foresight = 0.9), labels = best_labels) +
  scale_linetype_manual(values = c(hm = "dashed", perfect_foresight = "solid"), labels = best_labels) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Theoretical best against the historical mean",
       subtitle = paste("The theoretical best knows the sign of the realised",
                        "h-month log equity premium. Dashed line: historical mean."),
       x = NULL, y = "Cumulative return (%)",
       colour = NULL, linewidth = NULL, linetype = NULL)

ggsave(file.path(fig_dir(), "cum_performance.png"), g_perf, width = 10, height = 8.5, dpi = 150)
ggsave(file.path(fig_dir(), "cum_performance_best.png"), g_perf_best, width = 10, height = 8, dpi = 150)

# ---- 3. Trading + forecast evaluation table ---------------------------------
FCST_COLS <- c("n", "oos_r2", "dm_stat", "dm_p", "hit", "hit_bench",
               "pt_stat", "pt_p", "pct_yhat_pos")
ext_ev <- read.csv(file.path(TAB_DIR, "all_eval.csv"), stringsAsFactors = FALSE) |>
  filter(set == "ext", model %in% MODELS) |>
  rename(pct_yhat_pos = pct_in_mkt) |>
  mutate(h = as.integer(h)) |>
  select(model, h, all_of(FCST_COLS))
stopifnot(nrow(ext_ev) == length(MODELS) * length(HORIZONS))

# Historical-mean hit rate on the same origins (equals hit_bench)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))
hm_hit <- bench |>
  group_by(h) |>
  summarise(hm_hit = round(100 * hit_rate(y, yhat), 3), .groups = "drop") |>
  mutate(h = as.integer(h))
stopifnot(isTRUE(all.equal(
  hm_hit$hm_hit[match(ext_ev$h, hm_hit$h)], ext_ev$hit_bench)))

perf_tbl <- bind_rows(
  paths |> filter(model %in% MODELS, set == "ext"),
  paths |> filter(model %in% c("hm", "perfect_foresight"))
) |>
  group_by(model, h) |>
  group_modify(~ perf_summary(.x)) |>
  ungroup() |>
  mutate(h = as.integer(h)) |>
  left_join(ext_ev, by = c("model", "h")) |>
  left_join(hm_hit, by = "h") |>
  mutate(hit = if_else(model == "hm", hm_hit, hit),
         model = recode(model, hm = "historical mean",
                        perfect_foresight = "theoretical best"),
         final_wealth = round(final_wealth / 1e6, 3),
         across(c(cum_return, cagr, ann_vol, sharpe, max_drawdown, pct_in_mkt),
                ~ round(.x, 3))) |>
  select(-hm_hit) |>
  rename(final_wealth_m = final_wealth) |>
  arrange(h, desc(cum_return), model)

write.csv(perf_tbl, file.path(TAB_DIR, "cum_performance_ext.csv"), row.names = FALSE)
cat("Saved performance figures to", FIG_DIR, "and cum_performance_ext.csv to", TAB_DIR, "\n")
