# =============================================================================
# 01_data_prep.R  -  build the monthly forecasting dataset from Data2025.xlsx
#
# Output: data/processed/monthly.rds, a list with
#   $data : one row per forecast origin t (end of month t)
#           y1, y3, y12 = log equity premium summed over t+1..t+h  (targets)
#           ret_next, rf_next = simple S&P500 and T-bill returns in t+1
#                               (the backtest compounds these month by month)
#           predictors  = values known at the end of month t, plus their
#                         values 1, 3 and 12 months earlier (LAGS)
#   $sets : predictor sets  gw (classic 14)  and  ext (gw + newer monthly)
#           $vars = base predictors and their lags (gw 56, ext 120)
#           $base = base predictors only (gw 14, ext 30)
#           Definitions: README.md, section Predictors.
#   $notes: what was dropped and why
# plus output/tables/predictor_availability.csv
#
# Timing convention: row t pairs x_t with outcomes after t. Downstream code
# never shifts again. For h > 1, the last h-1 rows of any training window
# have unrealised targets at the forecast origin and must be dropped.
# =============================================================================

source("R/00_config.R")

# ---- Predictor definitions --------------------------------------------------
# Classic Goyal-Welch (2008). Inflation enters as infl_l1 = infl_{t-1}: CPI
# for month t is published in month t+1, so infl_t is not known at the end of
# month t (Goyal & Welch 2008, p. 1459, wait one month before using it).
GW_VARS <- c("dp", "dy", "ep", "de", "svar", "bm", "ntis",
             "tbl", "lty", "ltr", "tms", "dfy", "dfr", "infl_l1")

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
  disag  = "starts 1981-12 (I/B/E/S); would cut ~660 training months",
  infl   = "same-month CPI is not known at t -> replaced by infl_l1"
)

# Lags of every base predictor, added as extra predictors (as the embed() lags
# in the Lecture 4 code, func-rf.R). <v>_l<k> is the series v, k months
# earlier. infl_l1 is already one month back, so its lags are infl_l2,
# infl_l4 and infl_l13. Built on the full history before the TRAIN_START cut,
# so only ntis (which starts 1926-12) has missing lags in the sample.
LAGS <- c(1, 3, 12)
lag_name <- function(v, k)
  if (v == "infl_l1") paste0("infl_l", 1 + k) else paste0(v, "_l", k)
with_lags <- function(vars)
  c(vars, unlist(lapply(LAGS, \(k) vapply(vars, lag_name, "", k = k)), use.names = FALSE))

# ---- Helpers ----------------------------------------------------------------
roll_mean <- function(x, k) as.numeric(stats::filter(x, rep(1 / k, k), sides = 1))

# sum of x over t+1..t+h (NA where not fully observed)
lead_sum <- function(x, h) {
  out <- rep(0, length(x))
  for (j in seq_len(h)) out <- out + lead(x, j)
  out
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
    infl_l1 = lag(infl),

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

# Lagged predictors (LAGS above); df is sorted by date
BASE_VARS <- c(GW_VARS, NEW_VARS, TECH_VARS)
for (v in BASE_VARS) for (k in LAGS) df[[lag_name(v, k)]] <- lag(df[[v]], k)

# ---- Checks -----------------------------------------------------------------
chk <- df |> filter(!is.na(d_p))
stopifnot(max(abs(exp(chk$dp) - chk$d_p)) < 1e-6)            # logs match sheet
chk <- raw |> filter(!is.na(tms))
stopifnot(max(abs(chk$tms - (chk$lty - chk$tbl))) < 1e-8)    # sheet's tms
ok <- which(!is.na(df$y12))
stopifnot(isTRUE(all.equal(df$y1[ok], df$eq_prem[ok + 1])),   # y1 = next month
          isTRUE(all.equal(df$y12[ok],
                           sapply(ok, \(i) sum(df$eq_prem[(i + 1):(i + 12)])))))
n <- nrow(df)                                                  # lags line up
stopifnot(identical(df$dp_l12[13:n], df$dp[1:(n - 12)]),
          identical(df$infl_l1[2:n], df$infl[1:(n - 1)]),
          identical(df$infl_l13[14:n], df$infl[1:(n - 13)]))

# ---- Assemble ---------------------------------------------------------------
ALL_VARS <- with_lags(BASE_VARS)
out <- df |>
  select(date, y1, y3, y12, ret_next, rf_next, all_of(ALL_VARS)) |>
  filter(!is.na(y1))                                  # need at least y_{t+1}

avail <- bind_rows(lapply(ALL_VARS, function(v) {
  i <- which(!is.na(out[[v]]))
  tibble(var = v,
         group = case_when(v %in% with_lags(GW_VARS) ~ "gw",
                           v %in% with_lags(NEW_VARS) ~ "new", TRUE ~ "tech"),
         start = out$date[min(i)], end = out$date[max(i)], n = length(i),
         gaps = sum(is.na(out[[v]][min(i):max(i)])))
}))
stopifnot(all(avail$gaps == 0))

# Keep only the sample from TRAIN_START on (variables were constructed on the
# full history above, so lags / moving averages at the start are complete).
# Every downstream script - benchmark, models, backtest - uses this sample.
out <- out |> filter(date >= TRAIN_START)
stopifnot(min(out$date) == TRAIN_START)

sample_range <- function(base) {
  vars <- with_lags(base)
  d <- out |> select(date, all_of(vars)) |> drop_na()
  list(vars = vars, base = base, start = min(d$date), end = max(d$date),
       n = nrow(d))
}

res <- list(
  data  = out,
  sets  = list(gw  = sample_range(GW_VARS),
               ext = sample_range(BASE_VARS)),
  notes = list(dropped = DROPPED, train_start = TRAIN_START, lags = LAGS)
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
