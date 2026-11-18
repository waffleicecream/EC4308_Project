# Helpers shared by the benchmark and every model.

target_col <- function(h) paste0("y", h)

# Rows of the test-period forecast origins for horizon h: from FIRST_ORIGIN to
# the last origin whose h-month target ends by TEST_END.
test_origins <- function(data, h) {
  y <- data[[target_col(h)]]
  last_origin <- TEST_END %m-% months(h)
  which(data$date >= FIRST_ORIGIN & data$date <= last_origin & !is.na(y))
}

# Training rows at origin row i: the latest `window` rows whose h-month target
# (months s+1..s+h) is realised by month i, i.e. s <= i - h.
train_rows <- function(i, h, window) {
  last  <- i - h
  first <- max(1, last - window + 1)
  if (last < first) integer(0) else first:last
}

# Window length: ROLL_WINDOW, or all rows available at the first test origin.
roll_window <- function(data, h) {
  if (!is.null(ROLL_WINDOW)) return(ROLL_WINDOW)
  test_origins(data, h)[1] - h
}

# One row per (model, set, h, origin). y is the realised premium over t+1..t+h.
make_forecast_tbl <- function(data, idx, h, yhat, model, set) {
  y <- data[[target_col(h)]][idx]
  tibble(model = model, set = set, h = h, date = data$date[idx], y = y, yhat = yhat)
}

save_forecasts <- function(fc, name) {
  dir.create(FCST_DIR, showWarnings = FALSE, recursive = TRUE)
  saveRDS(fc, file.path(FCST_DIR, paste0(name, ".rds")))
}

# output/tables/<model>/, created if needed
model_tab_dir <- function(model) {
  d <- file.path(TAB_DIR, model)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}
