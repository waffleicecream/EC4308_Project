# Pseudo-out-of-sample (POOS) engine shared by every model and the benchmark.
#
# At each test origin t (FIRST_ORIGIN onward):
#   - training rows: the latest W rows whose h-month target is realised by t
#     (rows t-h-W+1 .. t-h), so nothing after t is used; W = roll_window()
#   - every RETUNE_EVERY origins, hyperparameters are re-tuned on that window
#   - the model is refit on the window at every origin
#   - y_h (premium over t+1..t+h) is forecast from x_t
#
# A model is a list with
#   name    : string, used in output file names
#   fit     : function(X, y, params) -> fitted object
#   predict : function(fit, X) -> numeric vector, one value per row of X
#   tune    : function(X, y, h) -> one-row data.frame of params (optional)
#   inspect : function(fit, X) -> named list of data.frames, logged at each
#             tune and saved as <name>_<element>.csv (optional)

# Split n training rows into [ fit block ][ gap of h-1 rows ][ last n_val rows ].
# The gap stops fit-block targets from overlapping validation targets.
val_split <- function(n, h, n_val = VAL_MONTHS) {
  stopifnot(n - n_val - (h - 1) > 0)
  list(fit = seq_len(n - n_val - (h - 1)), val = (n - n_val + 1):n)
}

# Fit each row of `grid` on the fit block and return the row with the lowest
# validation MSE (plus that val_mse).
tune_grid <- function(X, y, h, grid, fit, predict, n_val = VAL_MONTHS) {
  s <- val_split(nrow(X), h, n_val)
  grid <- as.data.frame(grid)
  mse <- vapply(seq_len(nrow(grid)), function(j) {
    m <- fit(X[s$fit, , drop = FALSE], y[s$fit], grid[j, , drop = FALSE])
    mean((y[s$val] - predict(m, X[s$val, , drop = FALSE]))^2)
  }, numeric(1))
  cbind(grid[which.min(mse), , drop = FALSE], val_mse = min(mse))
}

# One model x one predictor set x one horizon
poos <- function(model, data, vars, h, set, retune_every = RETUNE_EVERY) {
  stopifnot(is.function(model$fit), is.function(model$predict))

  yc   <- target_col(h)
  idx  <- test_origins(data, h)
  win  <- roll_window(data, h)
  yhat <- rep(NA_real_, length(idx))
  params <- NULL
  tune_log <- list(); insp_log <- list(); fit_log <- list()

  for (k in seq_along(idx)) {
    i <- idx[k]
    do_tune <- is.function(model$tune) && (k - 1) %% retune_every == 0

    tr <- data[train_rows(i, h, win), c("date", yc, vars), drop = FALSE] |> drop_na()
    X  <- as.matrix(tr[vars]); y <- tr[[yc]]
    info <- data.frame(model = model$name, set = set, h = h,
                       origin = data$date[i], train_start = min(tr$date),
                       train_end = max(tr$date), n_train = nrow(tr))

    if (do_tune) {
      params <- model$tune(X, y, h)
      tune_log[[length(tune_log) + 1]] <- cbind(info, params, row.names = NULL)
    }
    fit <- model$fit(X, y, params)
    fit_log[[length(fit_log) + 1]] <- info

    if (do_tune && is.function(model$inspect)) {
      ins <- model$inspect(fit, X)
      for (nm in names(ins)) if (!is.null(ins[[nm]]) && nrow(ins[[nm]]) > 0)
        insp_log[[nm]][[length(insp_log[[nm]]) + 1]] <-
          cbind(info[, 1:4], ins[[nm]], row.names = NULL)
    }

    x_t <- as.matrix(data[i, vars, drop = FALSE])
    stopifnot(!anyNA(x_t))
    yhat[k] <- model$predict(fit, x_t)
  }

  list(fc      = make_forecast_tbl(data, idx, h, yhat, model = model$name, set = set),
       tuning  = bind_rows(tune_log),
       inspect = lapply(insp_log, bind_rows),
       fits    = bind_rows(fit_log))
}

# Stack a list of poos() results into one
combine_runs <- function(runs) {
  insp_names <- unique(unlist(lapply(runs, \(r) names(r$inspect))))
  list(fc      = bind_rows(lapply(runs, `[[`, "fc")),
       tuning  = bind_rows(lapply(runs, `[[`, "tuning")),
       inspect = setNames(lapply(insp_names, \(nm)
                   bind_rows(lapply(runs, \(r) r$inspect[[nm]]))), insp_names),
       fits    = bind_rows(lapply(runs, `[[`, "fits")))
}

# Every predictor set (a named list like prep$sets) x every horizon
poos_all <- function(model, data, sets, horizons = HORIZONS) {
  t0 <- Sys.time()
  runs <- list()
  for (s in names(sets)) for (h in horizons) {
    r <- poos(model, data, sets[[s]]$vars, h, set = s)
    runs[[paste(s, h)]] <- r
    cat(sprintf("  %s: set=%s h=%d  %d forecasts, train %s..%s (n=%d) at first origin\n",
                model$name, s, h, nrow(r$fc),
                format(r$fits$train_start[1]), format(r$fits$train_end[1]),
                r$fits$n_train[1]))
  }
  cat("  elapsed:", format(round(Sys.time() - t0, 1)), "\n")
  combine_runs(runs)
}

# Forecasts to output/forecasts/<name>.rds; tuning and inspect logs to
# output/tables/<name>/
save_poos <- function(res, name) {
  save_forecasts(res$fc, name)
  dir <- model_tab_dir(name)
  if (nrow(res$tuning))
    write.csv(res$tuning, file.path(dir, paste0(name, "_tuning.csv")), row.names = FALSE)
  for (nm in names(res$inspect)) if (nrow(res$inspect[[nm]]))
    write.csv(res$inspect[[nm]], file.path(dir, paste0(name, "_", nm, ".csv")),
              row.names = FALSE)
}

# Evaluate res against the benchmark and write output/tables/<name>/<name>_eval.csv
save_eval <- function(res, name, bench) {
  ev <- evaluate_all(res$fc, bench)
  write.csv(ev, file.path(model_tab_dir(name), paste0(name, "_eval.csv")), row.names = FALSE)
  ev
}
