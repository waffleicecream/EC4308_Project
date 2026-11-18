# Prevailing-mean benchmark (Goyal and Welch 2008): the forecast at each
# origin is the mean h-month premium in the rolling training window. It runs
# through the same POOS engine, so it uses exactly the models' windows.
#
# Output: output/forecasts/hm.rds

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/poos.R")

d <- readRDS(PROC_FILE)$data
stopifnot(all(diff(d$date) %in% 28:31))   # contiguous months, so row index = time

hm_model <- list(
  name    = "hm",
  fit     = function(X, y, params) mean(y),
  predict = function(fit, X) rep(fit, nrow(X))
)

# Uses no predictors, so it gets its own set label "all"
res <- poos_all(hm_model, d, list(all = list(vars = character(0))), HORIZONS)
fc  <- res$fc

# Check against the window mean computed directly
for (h in HORIZONS) {
  y   <- d[[target_col(h)]]
  idx <- test_origins(d, h)
  W   <- roll_window(d, h)
  direct <- sapply(idx, \(i) mean(y[(i - h - W + 1):(i - h)]))
  stopifnot(isTRUE(all.equal(fc$yhat[fc$h == h], direct)))
}

save_forecasts(fc, "hm")

fc |>
  group_by(h) |>
  summarise(first_origin = min(date), last_origin = max(date), n = n(),
            mean_yhat = mean(yhat), mean_y = mean(y),
            pct_yhat_pos = 100 * mean(yhat > 0),
            mse = mean((y - yhat)^2), .groups = "drop") |>
  print(width = Inf)
