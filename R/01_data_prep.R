# Build the monthly forecasting dataset from data/Data2025.xlsx.
#
# Output: data/processed/monthly.rds, a list with
#   $data : one row per month-end t
#           y1, y3, y12       log equity premium summed over t+1..t+h (targets)
#           ret_next, rf_next simple S&P 500 and T-bill returns in t+1 (backtest)
#           predictors        known at the end of month t, plus their values
#                             1, 3 and 12 months earlier
#   $sets : $ext with $vars (30 base predictors + lags = 120) and $base (30)
#   $notes: dropped series and why
# and output/tables/predictor_availability.csv.
#
# Row t pairs x_t with outcomes after t; later scripts never shift again.
# Predictor definitions: README.md.

source("R/00_config.R")

# ---- Predictors -------------------------------------------------------------
# Goyal and Welch (2008). CPI for month t is published in t+1, so inflation
# enters as infl_l1 = infl_{t-1}.
GW_VARS <- c("dp", "dy", "ep", "de", "svar", "bm", "ntis",
             "tbl", "lty", "ltr", "tms", "dfy", "dfr", "infl_l1")

# Goyal, Welch and Zafirov (2024) series available from 1926
NEW_VARS <- c("lzrt", "wtexas", "skvw", "tail", "rdsp", "dtoy", "dtoat", "avgcor")

# Neely, Rapach, Tu and Zhou (2014) price-based technical indicators, rebuilt
# here because the workbook's tchi uses full-sample PCA loadings
TECH_VARS <- c(paste0("ma_", rep(1:3, each = 2), "_", c(9, 12)), "mom_9", "mom_12")

BASE_VARS <- c(GW_VARS, NEW_VARS, TECH_VARS)

DROPPED <- c(
  ogap   = "full-sample quadratic trend (look-ahead)",
  sntm   = "PLS-targeted at returns (look-ahead); ends 2023-11",
  fbm    = "PLS-targeted at returns (look-ahead)",
  tchi   = "full-sample PCA loadings; rebuilt as TECH_VARS",
  csp    = "ends 2002-12",
  ndrbl  = "ends 2024-12",
  vp     = "1990-2021 only",
  vrp    = "1990-2024 only",
  impvar = "1996-2025/08 only",
  rsvix  = "1996-2025/08 only",
  ygap   = "starts 1953-04; overlaps dp/ep and lty",
  shtint = "starts 1973-01; full-sample detrend (look-ahead)",
  disag  = "starts 1981-12",
  infl   = "same-month CPI is not known at t; replaced by infl_l1"
)

# <v>_l<k> is series v, k months earlier. infl_l1 is already one month back,
# so its lags are infl_l2, infl_l4 and infl_l13.
LAGS <- c(1, 3, 12)
lag_name <- function(v, k)
  if (v == "infl_l1") paste0("infl_l", 1 + k) else paste0(v, "_l", k)
with_lags <- function(vars)
  c(vars, unlist(lapply(LAGS, \(k) vapply(vars, lag_name, "", k = k)), use.names = FALSE))

roll_mean <- function(x, k) as.numeric(stats::filter(x, rep(1 / k, k), sides = 1))

lead_sum <- function(x, h) {            # sum of x over t+1..t+h
  out <- rep(0, length(x))
  for (j in seq_len(h)) out <- out + lead(x, j)
  out
}

# ---- Read -------------------------------------------------------------------
# guess_max matters: with the default, late-starting series are read as logical.
raw <- read_excel(RAW_FILE, sheet = "Monthly", guess_max = 1e5,
                  na = c("", "NA", "NaN"))
names(raw) <- gsub("/", "_", names(raw), fixed = TRUE)
raw <- raw |>
  mutate(date = ceiling_date(ymd(paste0(yyyymm, "01")), "month") - days(1)) |>
  arrange(date)
stopifnot(!anyDuplicated(raw$date))

# ---- Construct --------------------------------------------------------------
df <- raw |>
  mutate(
    eq_prem = log1p(ret) - log1p(Rfree),         # log equity premium in month t

    dp   = log(d12) - log(price),
    dy   = log(d12) - log(lag(price)),
    ep   = log(e12) - log(price),
    de   = log(d12) - log(e12),
    bm   = b_m,
    tms  = lty - tbl,
    dfy  = BAA - AAA,
    dfr  = corpr - ltr,
    infl_l1 = lag(infl),

    ma_1_9  = as.integer(price >= roll_mean(price, 9)),      # 1 = bullish
    ma_1_12 = as.integer(price >= roll_mean(price, 12)),
    ma_2_9  = as.integer(roll_mean(price, 2) >= roll_mean(price, 9)),
    ma_2_12 = as.integer(roll_mean(price, 2) >= roll_mean(price, 12)),
    ma_3_9  = as.integer(roll_mean(price, 3) >= roll_mean(price, 9)),
    ma_3_12 = as.integer(roll_mean(price, 3) >= roll_mean(price, 12)),
    mom_9   = as.integer(price >= lag(price, 9)),
    mom_12  = as.integer(price >= lag(price, 12)),

    y1       = lead_sum(eq_prem, 1),
    y3       = lead_sum(eq_prem, 3),
    y12      = lead_sum(eq_prem, 12),
    ret_next = lead(ret),
    rf_next  = lead(Rfree)
  )

# Lags are built on the full history, before the sample is cut at TRAIN_START
for (v in BASE_VARS) for (k in LAGS) df[[lag_name(v, k)]] <- lag(df[[v]], k)

# ---- Checks -----------------------------------------------------------------
chk <- df |> filter(!is.na(d_p))
stopifnot(max(abs(exp(chk$dp) - chk$d_p)) < 1e-6)            # logs match the sheet
chk <- raw |> filter(!is.na(tms))
stopifnot(max(abs(chk$tms - (chk$lty - chk$tbl))) < 1e-8)
ok <- which(!is.na(df$y12))
stopifnot(isTRUE(all.equal(df$y1[ok], df$eq_prem[ok + 1])),
          isTRUE(all.equal(df$y12[ok],
                           sapply(ok, \(i) sum(df$eq_prem[(i + 1):(i + 12)])))))
n <- nrow(df)
stopifnot(identical(df$dp_l12[13:n], df$dp[1:(n - 12)]),
          identical(df$infl_l1[2:n], df$infl[1:(n - 1)]),
          identical(df$infl_l13[14:n], df$infl[1:(n - 13)]))

# ---- Assemble ---------------------------------------------------------------
ALL_VARS <- with_lags(BASE_VARS)
out <- df |>
  select(date, y1, y3, y12, ret_next, rf_next, all_of(ALL_VARS)) |>
  filter(!is.na(y1))

avail <- bind_rows(lapply(ALL_VARS, function(v) {
  i <- which(!is.na(out[[v]]))
  tibble(var = v,
         group = case_when(v %in% with_lags(GW_VARS) ~ "gw",
                           v %in% with_lags(NEW_VARS) ~ "new", TRUE ~ "tech"),
         start = out$date[min(i)], end = out$date[max(i)], n = length(i),
         gaps = sum(is.na(out[[v]][min(i):max(i)])))
}))
stopifnot(all(avail$gaps == 0))

out <- out |> filter(date >= TRAIN_START)
stopifnot(min(out$date) == TRAIN_START)

complete <- out |> select(date, all_of(ALL_VARS)) |> drop_na()
res <- list(
  data  = out,
  sets  = list(ext = list(vars = ALL_VARS, base = BASE_VARS,
                          start = min(complete$date), end = max(complete$date),
                          n = nrow(complete))),
  notes = list(dropped = DROPPED, train_start = TRAIN_START, lags = LAGS)
)

dir.create(dirname(PROC_FILE), showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(res, PROC_FILE)
write.csv(avail, file.path(TAB_DIR, "predictor_availability.csv"), row.names = FALSE)

r <- res$sets$ext
cat(sprintf("ext: %d columns (%d base), complete rows %s to %s (%d)\n",
            length(r$vars), length(r$base), r$start, r$end, r$n))
cat("Last origin with y1 / y3 / y12:",
    format(max(out$date[!is.na(out$y1)])), "/",
    format(max(out$date[!is.na(out$y3)])), "/",
    format(max(out$date[!is.na(out$y12)])), "\n")
