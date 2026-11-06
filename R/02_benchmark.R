# =============================================================================
# 02_benchmark.R  -  prevailing-mean benchmark (Goyal & Welch 2008)
#
# The benchmark forecast of the h-month log equity premium at each origin is
# the average of the h-month premia in the rolling training window. It runs
# through the same POOS engine as every model, so it uses exactly the same
# training windows. It uses only y, so it is the same for the gw and ext sets.
#
# Output: output/forecasts/hm.rds (columns: model, set, h, date, y, yhat)
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/poos.R")

d <- readRDS(PROC_FILE)$data
stopifnot(all(diff(d$date) %in% 28:31))   # contiguous months -> index = time

hm_model <- list(
  name    = "hm",
  fit     = function(X, y, params) mean(y),
  predict = function(fit, X) rep(fit, nrow(X))
)

res <- poos_all(hm_model, d, list(all = list(vars = character(0))), HORIZONS)
fc  <- res$fc

# Spot-check: forecast at each origin = mean of the W realised targets ending
# h months before the origin, computed directly.
for (h in HORIZONS) {
  y   <- d[[target_col(h)]]
  idx <- test_origins(d, h)
  W   <- roll_window(d, h)
  direct <- sapply(idx, \(i) mean(y[(i - h - W + 1):(i - h)]))
  stopifnot(isTRUE(all.equal(fc$yhat[fc$h == h], direct)))
}

save_forecasts(fc, "hm")

# ---- Summary ----------------------------------------------------------------
fc |>
  group_by(h) |>
  summarise(first_origin = min(date),
            last_origin = max(date), n = n(),
            mean_yhat = mean(yhat), mean_y = mean(y),
            pct_yhat_pos = 100 * mean(yhat > 0),   # trading signal = "in market"
            mse = mean((y - yhat)^2), .groups = "drop") |>
  print(width = Inf)
