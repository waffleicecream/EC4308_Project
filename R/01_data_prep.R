# =============================================================================
# 01_data_prep.R  -  build the monthly forecasting dataset from Data2025.xlsx
#
# Output: data/processed/monthly.rds, a list with
#   $data : one row per forecast origin t (end of month t)
#           y1, y3, y12 = log equity premium summed over t+1..t+h  (targets)
#           ret_next, rf_next = simple S&P500 and T-bill returns in t+1
#                               (the backtest compounds these month by month)
#           predictors  = lags t-1..t-4 of each series. The column itself is
#                         the value known at the end of month t (lag t-1,
#                         relative to the return that starts in t+1).
#                         var_l2, var_l3, var_l4 are that series 1, 2, 3
#                         months earlier. infl_l5 is stored for the rf_lag /
#                         bag_lag runs and is not in either predictor set.
#   $sets : predictor sets  gw (classic 14 x 4 lags)  and  ext (30 x 4 lags)
#           Definitions: README.md, section Predictors.
#   $notes: what was dropped and why, plus max_lag
# plus output/tables/predictor_availability.csv
#
# Timing convention: row t pairs lags t-1..t-4 with outcomes after t.
# Downstream code never shifts again. For h > 1, the last h-1 rows of any
# training window have unrealised targets at the forecast origin and must
# be dropped.
# =============================================================================

source("R/00_config.R")

# ---- Predictor definitions --------------------------------------------------
# Classic Goyal-Welch (2008). 
GW_VARS <- c("dp", "dy", "ep", "de", "svar", "bm", "ntis",
             "tbl", "lty", "ltr", "tms", "dfy", "dfr", "infl")

# Newer GWZ (2024) monthly predictors kept as given.
# Only those available from 1926 (see DROPPED for the late starters).
NEW_VARS <- c("lzrt", "wtexas", "skvw", "tail", "rdsp", "dtoy", "dtoat",
              "avgcor")

# tchi in the file is PC1 of 14 indicators with full-sample loadings. We
# rebuild the 8 price-based indicators of Neely et al. (2014) instead; the
# 6 volume-based ones need volume data we do not have. Models can use them
# directly or take PC1 inside each window.
TECH_VARS <- c(paste0("ma_", rep(1:3, each = 2), "_", c(9, 12)),
               "mom_9", "mom_12")

DROPPED <- c(
  ogap   = "full-sample quadratic trend (look-ahead)",
  sntm   = "PLS-targeted at returns (severe look-ahead); ends 2023-11",
  fbm    = "PLS-targeted at returns (severe look-ahead)",
  tchi   = "full-sample PCA loadings -> rebuilt as TECH_VARS",
  csp    = "ends 2002-12",
  ndrbl  = "ends 2024-12",
  vp     = "1990-2021 only",
  vrp    = "1990-2024 only",
  impvar = "1996-2025/08 only",
  rsvix  = "1996-2025/08 only",
  ygap   = "starts 1953-04 (would cut the 1926 training start); overlaps dp/ep and lty",
  shtint = "starts 1973-01; full-sample detrend (look-ahead, per ReadMe)",
  disag  = "starts 1981-12 (I/B/E/S); would cut ~660 training months"
)

# ---- Helpers ----------------------------------------------------------------
roll_mean <- function(x, k) as.numeric(stats::filter(x, rep(1 / k, k), sides = 1))

# sum of x over t+1..t+h (NA where not fully observed)
lead_sum <- function(x, h) {
  out <- rep(0, length(x))
  for (j in seq_len(h)) out <- out + lead(x, j)
  out
}

# var_lk = the series k-1 months before this row (k = 2..MAX_LAG).
# k = 1 stays under the original name.
add_lags <- function(df, vars, max_lag = MAX_LAG) {
  stopifnot(max_lag >= 2)
  for (v in vars) for (k in 2:max_lag)
    df[[paste0(v, "_l", k)]] <- dplyr::lag(df[[v]], n = k - 1)
  df
}

# One base name, then its lags, in that order: dp, dp_l2, dp_l3, dp_l4, ...
expand_vars <- function(vars, max_lag = MAX_LAG) {
  unlist(lapply(vars, function(v) c(v, paste0(v, "_l", 2:max_lag))),
         use.names = FALSE)
}

# ---- Read -------------------------------------------------------------------
# guess_max is essential: with readxl's default, late-starting series are
# read as logical and silently corrupted.
raw <- read_excel(RAW_FILE, sheet = "Monthly", guess_max = 1e5,
                  na = c("", "NA", "NaN"))
names(raw) <- gsub("/", "_", names(raw), fixed = TRUE)
raw <- raw |>
  mutate(date = ceiling_date(ymd(paste0(yyyymm, "01")), "month") - days(1)) |>
  arrange(date)
stopifnot(!anyDuplicated(raw$date))

# ---- Construct variables ----------------------------------------------------
df <- raw |>
  mutate(
    eq_prem = log1p(ret) - log1p(Rfree),         # log equity premium in month t

    # Goyal-Welch definitions (valuation ratios in logs)
    dp   = log(d12) - log(price),
    dy   = log(d12) - log(lag(price)),
    ep   = log(e12) - log(price),
    de   = log(d12) - log(e12),
    bm   = b_m,
    tms  = lty - tbl,
    dfy  = BAA - AAA,
    dfr  = corpr - ltr,

    # Neely et al. (2014) price-based technical indicators (1 = bullish)
    ma_1_9  = as.integer(price >= roll_mean(price, 9)),
    ma_1_12 = as.integer(price >= roll_mean(price, 12)),
    ma_2_9  = as.integer(roll_mean(price, 2) >= roll_mean(price, 9)),
    ma_2_12 = as.integer(roll_mean(price, 2) >= roll_mean(price, 12)),
    ma_3_9  = as.integer(roll_mean(price, 3) >= roll_mean(price, 9)),
    ma_3_12 = as.integer(roll_mean(price, 3) >= roll_mean(price, 12)),
    mom_9   = as.integer(price >= lag(price, 9)),
    mom_12  = as.integer(price >= lag(price, 12)),

    # Targets and backtest inputs
    y1       = lead_sum(eq_prem, 1),
    y3       = lead_sum(eq_prem, 3),
    y12      = lead_sum(eq_prem, 12),
    ret_next = lead(ret),
    rf_next  = lead(Rfree)
  )

# ---- Checks -----------------------------------------------------------------
chk <- df |> filter(!is.na(d_p))
stopifnot(max(abs(exp(chk$dp) - chk$d_p)) < 1e-6)            # logs match sheet
chk <- raw |> filter(!is.na(tms))
stopifnot(max(abs(chk$tms - (chk$lty - chk$tbl))) < 1e-8)    # sheet's tms
ok <- which(!is.na(df$y12))
stopifnot(isTRUE(all.equal(df$y1[ok], df$eq_prem[ok + 1])),   # y1 = next month
          isTRUE(all.equal(df$y12[ok],
                           sapply(ok, \(i) sum(df$eq_prem[(i + 1):(i + 12)])))))

# ---- Assemble ---------------------------------------------------------------
# Lags are taken on the full history, before any row is dropped, so the first
# kept month still has its t-4 value. infl_l5 (lag t-5) is only for the
# publication-lag forests; it is not a member of either predictor set.
ALL_VARS  <- c(GW_VARS, NEW_VARS, TECH_VARS)
PRED_VARS <- expand_vars(ALL_VARS)
df <- add_lags(df, ALL_VARS)
df$infl_l5 <- dplyr::lag(df$infl, n = MAX_LAG)

out <- df |>
  select(date, y1, y3, y12, ret_next, rf_next, all_of(PRED_VARS), infl_l5) |>
  filter(!is.na(y1))                                  # need at least y_{t+1}

avail <- bind_rows(lapply(ALL_VARS, function(v) {
  i <- which(!is.na(out[[v]]))
  tibble(var = v,
         group = case_when(v %in% GW_VARS ~ "gw", v %in% NEW_VARS ~ "new",
                           TRUE ~ "tech"),
         start = out$date[min(i)], end = out$date[max(i)], n = length(i),
         gaps = sum(is.na(out[[v]][min(i):max(i)])))
}))
stopifnot(all(avail$gaps == 0))

# Keep only the sample from TRAIN_START on (variables were constructed on the
# full history above, so lags / moving averages at the start are complete).
# Every downstream script - benchmark, models, backtest - uses this sample.
out <- out |>
  filter(date >= TRAIN_START) |>
  filter(if_all(all_of(c(PRED_VARS, "infl_l5")), ~ !is.na(.x)))
stopifnot(min(out$date) == TRAIN_START, !anyNA(out[c(PRED_VARS, "infl_l5")]))

# var_lk on this row is the base series k-1 rows earlier (the panel has no gaps).
n <- nrow(out)
stopifnot(
  isTRUE(all.equal(out$dp_l2[-1], out$dp[-n])),
  isTRUE(all.equal(out$dp_l3[-c(1, 2)], out$dp[seq_len(n - 2)])),
  isTRUE(all.equal(out$dp_l4[-c(1, 2, 3)], out$dp[seq_len(n - 3)])),
  isTRUE(all.equal(out$infl_l5[-c(1:4)], out$infl[seq_len(n - 4)])),
  max(abs(out$de_l4 - (out$dp_l4 - out$ep_l4))) < 1e-8,
  max(abs(out$tms_l4 - (out$lty_l4 - out$tbl_l4))) < 1e-8
)

sample_range <- function(vars) {
  d <- out |> select(date, all_of(vars)) |> drop_na()
  list(vars = vars, start = min(d$date), end = max(d$date), n = nrow(d))
}

res <- list(
  data  = out,
  sets  = list(gw  = sample_range(expand_vars(GW_VARS)),
               ext = sample_range(PRED_VARS)),
  notes = list(dropped = DROPPED, train_start = TRAIN_START, max_lag = MAX_LAG)
)

dir.create(dirname(PROC_FILE), showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(res, PROC_FILE)
write.csv(avail, file.path(TAB_DIR, "predictor_availability.csv"), row.names = FALSE)

# ---- Summary ----------------------------------------------------------------
for (s in names(res$sets)) {
  r <- res$sets[[s]]
  cat(sprintf("Set %-3s: %2d vars, %s to %s, %d origins\n",
              s, length(r$vars), r$start, r$end, r$n))
}
cat("Targets: y1/y3/y12 last origin =",
    format(max(out$date[!is.na(out$y1)])), "/",
    format(max(out$date[!is.na(out$y3)])), "/",
    format(max(out$date[!is.na(out$y12)])), "\n")
