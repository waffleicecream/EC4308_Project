# =============================================================================
# 12_performance_plots.R  -  target over time, and cumulative trading performance
#
# 1. Target. Log equity premium at each horizon (y1, y3, y12): the sum of
#    monthly log premia over t+1..t+h, dated at the end of that window.
#    The historical mean is the prevailing mean of realised targets known at
#    that origin (02_benchmark.R). The window is expanding until it reaches
#    the test-period length, then rolling, so on the test set the line is the
#    benchmark forecast. Full sample, with the test window (TEST_START..TEST_END) shaded.
#
# 2. Cumulative performance, ext predictor set only, over the test set.
#    Models are drawn against the historical-mean strategy (10_trading.R).
#    That strategy stays in the market, so its wealth path matches buy-and-hold.
#    T-bills are the dotted line. A second figure compares the theoretical
#    best (long the S&P 500 only when the realised h-month log equity premium
#    is positive) with the same historical-mean baseline.
#
# Outputs: output/figures/target_equity_premium.png
#          output/figures/cum_performance.png
#          output/figures/cum_performance_best.png
#          output/tables/cum_performance_ext.csv
#            ext-set trading results plus the forecast evaluation
#            (oos_r2, DM test, hit rate, PT test). pct_in_mkt is the share
#            of test months in the S&P 500. pct_yhat_pos is the share of
#            forecast origins with a positive forecast; the two agree at
#            h = 1 and differ when the backtest only trades every h months.
# Run 01_data_prep.R and 10_trading.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/trading.R")
source("R/functions/metrics.R")
suppressPackageStartupMessages(library(ggplot2))

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Style (same as 11_forecast_plots.R) -----------------------------------
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

# Prevailing mean at origin i: mean of realised targets in rows
# i-h-W+1 .. i-h (the benchmark window), truncated at the start of the sample.
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
  y <- d[[target_col(hh)]]
  yhat <- prevailing_mean(y, hh, roll_window(d, hh))
  saved <- hm_fc %>% filter(h == hh) %>% arrange(set, date)
  by_set <- split(saved$yhat, saved$set)
  stopifnot(length(unique(lengths(by_set))) == 1,
            all(vapply(by_set, function(v) isTRUE(all.equal(v, by_set[[1]])),
                       logical(1))))
  idx <- match(unique(saved$date), d$date)
  stopifnot(!anyNA(idx), isTRUE(all.equal(yhat[idx], by_set[[1]])))

  date <- realised_end(d$date, hh)
  ok <- !is.na(y)
  bind_rows(
    tibble(h = hh, date = date[ok], value = 100 * y[ok],    series = "Realised"),
    tibble(h = hh, date = date[ok], value = 100 * yhat[ok], series = "Historical mean")
  )
})) %>%
  filter(!is.na(value)) %>%
  mutate(hl = h_factor(h),
         series = factor(series, levels = c("Realised", "Historical mean")))
stopifnot(!anyDuplicated(series %>% filter(series == "Realised") %>% select(h, date)))

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

ggsave(file.path(FIG_DIR, "target_equity_premium.png"), g_target,
       width = 10, height = 8, dpi = 150)

# ---- 2. Cumulative performance ----------------------------------------------
paths <- readRDS(file.path(TAB_DIR, "trading_paths.rds"))

present <- setdiff(unique(paths$model),
                   c("perfect_foresight", "buy_hold", "tbill", "hm"))
preferred <- c("ensemble_mean", "rf", "bag", "xgb",
               "lasso_bic", "rlasso", "post_lasso", "pca_lasso", "pcr_rlasso",
               "ridge", "kitchen_sink")
MODELS <- c(intersect(preferred, present), sort(setdiff(present, preferred)))

# Draw the baseline last so it stays visible where a model sits on it.
# Legend order is set separately below.
series_levels <- c(MODELS, "tbill", "hm")
series_breaks <- c("hm", "tbill", MODELS)
series_labels <- c(hm = "historical mean", tbill = "T-bills",
                   setNames(MODELS, MODELS))

model_cols <- c(
  ensemble_mean     = "#c0392b",
  rf                = "#2a78d6",
  bag               = "#d35400",
  xgb               = "#8e44ad",
  lasso_bic         = "#1e8449",
  rlasso            = "#27ae60",
  post_lasso        = "#0e6655",
  pca_lasso         = "#b7950b",
  pcr_rlasso        = "#7d6608",
  ridge             = "#5d6d7e",
  kitchen_sink      = "#1a5276",
  hm                = INK,
  tbill             = GREY,
  perfect_foresight = "#1a1a1a"
)
extra <- setdiff(MODELS, names(model_cols))
if (length(extra)) {
  model_cols <- c(model_cols,
                  setNames(scales::hue_pal()(length(extra)), extra))
}

to_cum <- function(p) {
  p %>% transmute(h, date, model, cum = 100 * (wealth / W0 - 1))
}

# Start every path at 0 on the eve of the test period.
add_zero <- function(p) {
  bind_rows(
    p %>% distinct(h, hl, series) %>% mutate(date = FIRST_ORIGIN, cum = 0),
    p
  )
}

mods <- paths %>%
  filter(model %in% MODELS, set == "ext") %>%
  to_cum()
refs <- paths %>%
  filter(model %in% c("hm", "tbill")) %>%
  to_cum()

perf <- bind_rows(mods, refs) %>%
  mutate(series = factor(model, levels = series_levels),
         hl     = h_factor(h)) %>%
  add_zero()

line_w <- c(setNames(rep(0.6, length(MODELS)), MODELS),
            hm = 0.8, tbill = 0.7)
line_t <- c(setNames(rep("solid", length(MODELS)), MODELS),
            hm = "dashed", tbill = "dotted")

g_perf <- ggplot(perf, aes(date, cum, colour = series,
                           linewidth = series, linetype = series)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ hl, ncol = 1) +
  scale_colour_manual(values = model_cols, breaks = series_breaks,
                      labels = series_labels) +
  scale_linewidth_manual(values = line_w, breaks = series_breaks,
                         labels = series_labels) +
  scale_linetype_manual(values = line_t, breaks = series_breaks,
                        labels = series_labels) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Cumulative trading performance",
       subtitle = paste(paste0("Ext set, ", format(TEST_START, "%Y"), "-",
                               format(TEST_END, "%Y"), ","),
                        "against the historical-mean strategy.",
                        "Dotted line: T-bills."),
       x = NULL, y = "Cumulative return (%)",
       colour = NULL, linewidth = NULL, linetype = NULL) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.text = element_text(size = 8)) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE,
                               override.aes = list(linewidth = 0.9)))

# Theoretical best against the same historical-mean baseline.
best_levels <- c("hm", "perfect_foresight")
best <- paths %>%
  filter(model %in% best_levels) %>%
  to_cum() %>%
  mutate(series = factor(model, levels = best_levels),
         hl     = h_factor(h)) %>%
  add_zero()

g_perf_best <- ggplot(best, aes(date, cum, colour = series,
                                linewidth = series, linetype = series)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ hl, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = model_cols,
                      labels = c(hm = "historical mean",
                                 perfect_foresight = "theoretical best")) +
  scale_linewidth_manual(values = c(hm = 0.8, perfect_foresight = 0.9),
                         labels = c(hm = "historical mean",
                                    perfect_foresight = "theoretical best")) +
  scale_linetype_manual(values = c(hm = "dashed", perfect_foresight = "solid"),
                        labels = c(hm = "historical mean",
                                   perfect_foresight = "theoretical best")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Theoretical best against the historical mean",
       subtitle = paste("The theoretical best knows the sign of the realised",
                        "h-month log equity premium. Dashed line: historical mean."),
       x = NULL, y = "Cumulative return (%)",
       colour = NULL, linewidth = NULL, linetype = NULL) +
  theme(legend.position = "top", legend.justification = "left")

ggsave(file.path(FIG_DIR, "cum_performance.png"), g_perf,
       width = 10, height = 8.5, dpi = 150)
ggsave(file.path(FIG_DIR, "cum_performance_best.png"), g_perf_best,
       width = 10, height = 8, dpi = 150)

# Trading results for the plotted ext-set models, plus forecast evaluation
# against the historical mean. The forecast columns are checked against a
# fresh evaluate_all() and against output/tables/all_eval.csv before joining.
FCST_COLS <- c("n", "oos_r2", "dm_stat", "dm_p", "hit", "hit_bench",
               "pt_stat", "pt_p", "pct_yhat_pos")
ext_ev <- read.csv(file.path(TAB_DIR, "all_eval.csv"), stringsAsFactors = FALSE) %>%
  filter(set == "ext", model %in% MODELS) %>%
  rename(pct_yhat_pos = pct_in_mkt) %>%
  mutate(model = as.character(model), h = as.integer(h)) %>%
  select(model, h, all_of(FCST_COLS))

bench <- readRDS(file.path(FCST_DIR, "hm.rds"))
fc_ext <- bind_rows(lapply(MODELS, function(m) {
  x <- readRDS(file.path(FCST_DIR, paste0(m, ".rds")))
  stopifnot(m %in% x$model)
  x %>% filter(model == m, set == "ext")
}))
fresh <- evaluate_all(fc_ext, bench) %>%
  filter(set == "ext") %>%
  rename(pct_yhat_pos = pct_in_mkt) %>%
  mutate(model = as.character(model), h = as.integer(h)) %>%
  select(model, h, all_of(FCST_COLS)) %>%
  arrange(model, h)
chk <- ext_ev %>% arrange(model, h)
stopifnot(nrow(fresh) == length(MODELS) * length(HORIZONS),
          identical(chk$model, fresh$model),
          identical(chk$h, fresh$h),
          isTRUE(all.equal(chk[FCST_COLS], fresh[FCST_COLS],
                           tolerance = 1e-6, check.attributes = FALSE)))

# Hit rate of the benchmark forecast on the same ext origins the models use.
# Rounded to 3 decimals, as in evaluate_all(). Must equal hit_bench.
origins <- fc_ext %>% distinct(h, date)
stopifnot(!anyDuplicated(bench[c("h", "date")]))
hm_hit <- bench %>%
  inner_join(origins, by = c("h", "date")) %>%
  group_by(h) %>%
  summarise(hm_hit = round(100 * hit_rate(y, yhat), 3),
            n_origin = n(), .groups = "drop") %>%
  mutate(h = as.integer(h))
bench_hit <- ext_ev %>%
  group_by(h) %>%
  summarise(hit_bench = unique(hit_bench), n_hit = n_distinct(hit_bench),
            .groups = "drop")
stopifnot(nrow(hm_hit) == length(HORIZONS),
          all(bench_hit$n_hit == 1),
          isTRUE(all.equal(hm_hit$hm_hit[match(bench_hit$h, hm_hit$h)],
                           bench_hit$hit_bench)))

shown <- bind_rows(
  paths %>% filter(model %in% MODELS, set == "ext"),
  paths %>% filter(model %in% c("hm", "perfect_foresight"))
)
perf_tbl <- shown %>%
  group_by(model, h) %>%
  group_modify(~ perf_summary(.x)) %>%
  ungroup() %>%
  mutate(h = as.integer(h)) %>%
  left_join(ext_ev, by = c("model", "h")) %>%
  mutate(model = recode(model,
                        hm = "historical mean",
                        perfect_foresight = "theoretical best"),
         final_wealth = round(final_wealth / 1e6, 3),
         across(c(cum_return, cagr, ann_vol, sharpe, max_drawdown, pct_in_mkt),
                ~ round(.x, 3))) %>%
  arrange(h, desc(cum_return), model) %>%
  rename(final_wealth_m = final_wealth) %>%
  left_join(hm_hit %>% select(h, hm_hit), by = "h") %>%
  mutate(hit = if_else(model == "historical mean", hm_hit, hit)) %>%
  select(-hm_hit)
stopifnot(nrow(perf_tbl) == (length(MODELS) + 2) * length(HORIZONS),
          !anyDuplicated(perf_tbl[c("model", "h")]),
          all(!is.na(perf_tbl$oos_r2[perf_tbl$model %in% MODELS])),
          all(is.na(perf_tbl$oos_r2[perf_tbl$model %in%
                                      c("historical mean", "theoretical best")])),
          all(!is.na(perf_tbl$hit[perf_tbl$model == "historical mean"])),
          isTRUE(all.equal(
            perf_tbl$hit[perf_tbl$model == "historical mean"][order(perf_tbl$h[perf_tbl$model == "historical mean"])],
            hm_hit$hm_hit[order(hm_hit$h)])))

write.csv(perf_tbl, file.path(TAB_DIR, "cum_performance_ext.csv"), row.names = FALSE)

cat("Saved target_equity_premium.png, cum_performance.png and",
    "cum_performance_best.png to", FIG_DIR, "\n")
cat("Saved cum_performance_ext.csv to", TAB_DIR, "\n")
