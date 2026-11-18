# Market-timing backtest: switch between the S&P 500 and T-bills.
#
# Every h months, at decision date t: hold 100% S&P 500 (total return) over
# t+1..t+h if the forecast made at t is positive, otherwise 100% T-bills.
# A switch costs COST of wealth. Start in T-bills, so entering the market at
# the first decision pays the cost. Long-only, no leverage. Wealth is tracked
# monthly, so every horizon uses the same 1996-01..2025-12 return series.

# Test-period origin rows; row t carries ret_next / rf_next, earned in month t+1.
test_months <- function(d) {
  which(d$date >= FIRST_ORIGIN & d$date <= (TEST_END %m-% months(1)))
}

# position: 0/1 (or logical) per decision date
backtest_switch <- function(d, decision_dates, position, h,
                            cost = COST, w0 = W0, start_pos = 0) {
  m   <- test_months(d)
  dts <- d$date[m]
  stopifnot(decision_dates[1] == dts[1],
            all(decision_dates %in% dts),
            length(dts) == h * length(decision_dates))

  k   <- findInterval(seq_along(dts), match(decision_dates, dts))
  pos <- as.integer(position)[k]                 # latest decision applies

  ret <- d$ret_next[m]; rf <- d$rf_next[m]
  switch_ <- pos != c(start_pos, head(pos, -1))
  gross   <- ifelse(pos == 1, 1 + ret, 1 + rf) * (1 - cost * switch_)
  wealth  <- w0 * cumprod(gross)

  tibble(date = ceiling_date(dts + days(1), "month") - days(1),  # month earned
         pos = pos, switch = switch_,
         ret = gross - 1, rf = rf, wealth = wealth)
}

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
