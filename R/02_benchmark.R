# =============================================================================
# 02_benchmark.R  -  prevailing-mean benchmark (Goyal & Welch 2008)
#
# At each forecast origin t, the benchmark forecast of the h-month log equity
# premium is the average of all h-month premia already realised by t, using
# the full history back to the start of the data (expanding window).
# It uses only y, so it is the same for the gw and ext predictor sets.
#
# Output: output/forecasts/hm.rds (columns: model, set, h, date, y, yhat)
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")

d <- readRDS(PROC_FILE)$data
stopifnot(all(diff(d$date) %in% 28:31))   # contiguous months -> index = time

prevailing_mean <- function(y, idx, h) {
  ok <- !is.na(y)
  cs <- cumsum(ifelse(ok, y, 0))          # running sum of realised targets
  cn <- cumsum(ok)                        # running count
  last <- idx - h                         # last row whose target is realised
  stopifnot(all(last >= 1), all(cn[last] > 0))
  cs[last] / cn[last]
}

fc <- bind_rows(lapply(HORIZONS, function(h) {
  idx  <- test_origins(d, h)
  yhat <- prevailing_mean(d[[target_col(h)]], idx, h)

  # Spot-check one origin against a direct computation
  j <- idx[1]; rows <- train_rows(j, h)
  stopifnot(isTRUE(all.equal(yhat[1], mean(d[[target_col(h)]][rows], na.rm = TRUE))))

  make_forecast_tbl(d, idx, h, yhat, model = "hm", set = "all")
}))

save_forecasts(fc, "hm")

# ---- Summary ----------------------------------------------------------------
fc |>
  group_by(h) |>
  summarise(first_origin = min(date), last_origin = max(date), n = n(),
            mean_yhat = mean(yhat), mean_y = mean(y),
            pct_yhat_pos = 100 * mean(yhat > 0),   # trading signal = "in market"
            mse = mean((y - yhat)^2), .groups = "drop") |>
  print(width = Inf)
