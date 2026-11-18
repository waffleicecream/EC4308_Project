# Forecast evaluation against the prevailing-mean benchmark.

# Campbell-Thompson out-of-sample R^2: positive = model beats the benchmark.
oos_r2 <- function(y, yhat, yhat_bench) {
  1 - sum((y - yhat)^2) / sum((y - yhat_bench)^2)
}

# Diebold-Mariano test, one-sided (H1: model MSE < benchmark MSE). Bartlett
# long-run variance with h-1 lags for the overlapping targets, plus the
# Harvey-Leybourne-Newbold small-sample correction.
dm_test <- function(y, yhat, yhat_bench, h) {
  d <- (y - yhat_bench)^2 - (y - yhat)^2
  n <- length(d); dbar <- mean(d); dc <- d - dbar
  lrv <- mean(dc^2)
  if (h > 1) for (l in 1:(h - 1)) {
    gl  <- sum(dc[(l + 1):n] * dc[1:(n - l)]) / n
    lrv <- lrv + 2 * (1 - l / h) * gl
  }
  stat <- dbar / sqrt(lrv / n)
  stat <- stat * sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  c(dm_stat = stat, dm_p = 1 - pt(stat, df = n - 1))
}

hit_rate <- function(y, yhat) mean((y > 0) == (yhat > 0))

# Pesaran-Timmermann (1992) directional-accuracy test, one-sided. NA when the
# forecast never changes sign.
pt_test <- function(y, yhat) {
  n  <- length(y)
  py <- mean(y > 0); px <- mean(yhat > 0)
  p_hat  <- hit_rate(y, yhat)
  p_star <- py * px + (1 - py) * (1 - px)
  v_p    <- p_star * (1 - p_star) / n
  v_star <- (2 * py - 1)^2 * px * (1 - px) / n +
            (2 * px - 1)^2 * py * (1 - py) / n +
            4 * py * px * (1 - py) * (1 - px) / n^2
  if (v_p - v_star <= 0) return(c(pt_stat = NA, pt_p = NA))
  stat <- (p_hat - p_star) / sqrt(v_p - v_star)
  c(pt_stat = stat, pt_p = 1 - pnorm(stat))
}

# One row per (model, set, h) in fc
evaluate_all <- function(fc, bench) {
  bind_rows(lapply(split(fc, list(fc$model, fc$set, fc$h), drop = TRUE), function(f) {
    hh <- f$h[1]
    stats <- evaluate_vs_bench(f, bench |> filter(h == hh), hh)
    tibble(model = f$model[1], set = f$set[1], h = hh, !!!as.list(round(stats, 3)))
  })) |> arrange(model, set, h)
}

evaluate_vs_bench <- function(fc, bench, h) {
  m <- inner_join(fc, bench |> select(date, yhat_bench = yhat), by = "date")
  c(n          = nrow(m),
    oos_r2     = 100 * oos_r2(m$y, m$yhat, m$yhat_bench),
    dm_test(m$y, m$yhat, m$yhat_bench, h),
    hit        = 100 * hit_rate(m$y, m$yhat),
    hit_bench  = 100 * hit_rate(m$y, m$yhat_bench),
    pt_test(m$y, m$yhat),
    pct_in_mkt = 100 * mean(m$yhat > 0))
}
