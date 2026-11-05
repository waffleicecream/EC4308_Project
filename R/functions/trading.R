# =============================================================================
# trading.R  -  market-timing backtest (switch between S&P 500 and T-bills)
#
# Rule, for rebalance frequency h months:
#   at each decision date t (every h months from FIRST_ORIGIN), look at the
#   forecast of the h-month log equity premium made at t:
#     forecast > 0  ->  hold 100% S&P 500 (total return) for months t+1..t+h
#     forecast <= 0 ->  hold 100% T-bills                for months t+1..t+h
#   Switching position costs COST = 2 x 0.1% = 0.2% of wealth (sell one asset,
#   buy the other). Start: W0 in T-bills, so entering the market at the first
#   decision also pays the cost. Long-only, no leverage, no shorting.
# Wealth is tracked monthly, so all horizons are compared on the same
# 1997-01 .. 2025-12 monthly return series.
# =============================================================================

# Months of the test period as origin rows: row t carries ret_next/rf_next,
# the returns earned in month t+1.
test_months <- function(d) {
  which(d$date >= FIRST_ORIGIN & d$date <= (TEST_END %m-% months(1)))
}

# signal_at: named logical/0-1 vector of positions, names = decision dates
backtest_switch <- function(d, decision_dates, position, h,
                            cost = COST, w0 = W0, start_pos = 0) {
  m   <- test_months(d)
  dts <- d$date[m]
  stopifnot(decision_dates[1] == dts[1],
            all(decision_dates %in% dts),
            length(dts) == h * length(decision_dates))

  # position held in each month = decision of the latest decision date
  k   <- findInterval(seq_along(dts), match(decision_dates, dts))
  pos <- as.integer(position)[k]

  ret <- d$ret_next[m]; rf <- d$rf_next[m]
  switch_ <- pos != c(start_pos, head(pos, -1))
  gross   <- ifelse(pos == 1, 1 + ret, 1 + rf) * (1 - cost * switch_)
  wealth  <- w0 * cumprod(gross)

  tibble(date = ceiling_date(dts + days(1), "month") - days(1),  # month earned
         pos = pos, switch = switch_,
         ret = gross - 1, rf = rf, wealth = wealth)
}

# Performance summary of a monthly backtest path
perf_summary <- function(p, w0 = W0) {
  n  <- nrow(p)
  ex <- p$ret - p$rf
  w  <- c(w0, p$wealth)
  tibble(final_wealth = p$wealth[n],
         cum_return   = 100 * (p$wealth[n] / w0 - 1),
         cagr         = 100 * ((p$wealth[n] / w0)^(12 / n) - 1),
         ann_vol      = 100 * sd(p$ret) * sqrt(12),
         sharpe       = if (sd(ex) > 1e-10) mean(ex) / sd(ex) * sqrt(12) else NA_real_,
         max_drawdown = 100 * max(1 - w / cummax(w)),
         n_switches   = sum(p$switch),
         pct_in_mkt   = 100 * mean(p$pos))
}
