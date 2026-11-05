# =============================================================================
# forecast_utils.R  -  helpers shared by the benchmark and all model scripts
# =============================================================================

# Name of the target column for horizon h
target_col <- function(h) paste0("y", h)

# Row indices of the test-period forecast origins for horizon h:
# origins from FIRST_ORIGIN until the last origin whose h-month target ends
# by TEST_END (and is observed).
test_origins <- function(data, h) {
  y <- data[[target_col(h)]]
  last_origin <- TEST_END %m-% months(h)
  which(data$date >= FIRST_ORIGIN & data$date <= last_origin & !is.na(y))
}

# Rows usable for training at origin row i for horizon h: their h-month target
# (months s+1..s+h) must be fully realised by month i, i.e. s <= i - h.
# window = Inf gives an expanding window (all history).
train_rows <- function(i, h, window = Inf) {
  last <- i - h
  first <- if (is.finite(window)) max(1, last - window + 1) else 1
  if (last < first) integer(0) else first:last
}

# Standard forecast output: one row per (model, set, h, origin)
make_forecast_tbl <- function(data, idx, h, yhat, model, set) {
  y_true <- data[[target_col(h)]][idx]         # realised log premium, t+1..t+h
  tibble(model = model, set = set, h = h,
         date  = data$date[idx],               # forecast origin t
         y     = y_true,
         yhat  = yhat)
}

save_forecasts <- function(fc, name) {
  dir.create(FCST_DIR, showWarnings = FALSE, recursive = TRUE)
  saveRDS(fc, file.path(FCST_DIR, paste0(name, ".rds")))
}
