# =============================================================================
# poos.R  -  shared pseudo-out-of-sample (POOS) engine for EVERY model
#
# Usage in a model script:
#   source("R/00_config.R"); source("R/functions/forecast_utils.R")
#   source("R/functions/metrics.R"); source("R/functions/poos.R")
#   res <- poos_all(my_model, d, prep$sets, HORIZONS)
#   save_poos(res, "my_model")
#
# ---- Scheme: rolling window ----------------------------------------------------
# At each test origin t (end of month t, FIRST_ORIGIN onward):
#   - training rows = the most recent W rows whose h-month target is fully
#     realised by t (rows t-h-W+1 .. t-h), so no data after t is ever used.
#     W = roll_window() (ROLL_WINDOW in 00_config.R); each step forward adds
#     one month at the end and drops one at the start
#   - hyperparameters are re-tuned every RETUNE_EVERY origins on the current
#     training window (see val_split())
#   - the model is re-estimated on the training window at every origin
#   - the forecast of y_h (premium over t+1..t+h) is made from lags t-1..t-4
#     of every predictor (x_t, x_{t-1}, x_{t-2}, x_{t-3}; see 01_data_prep.R)
#
# ---- Model interface ----------------------------------------------------------
# A model is a list with
#   name    : string, used in output files and tables
#   fit     : function(X, y, params) -> fitted object                (required)
#   predict : function(fit, X) -> numeric vector, one per row of X    (required)
#   tune    : function(X, y, h) -> one-row data.frame of params        (optional;
#             omit for models with nothing to tune; params is then NULL)
#   inspect : function(fit, X) -> named list of data.frames, logged at each
#             tune (optional; e.g. list(importance = ..., pdp = ...)). X is the
#             training matrix the model was fitted on. Each element is saved
#             to <name>_<element>.csv.
# X is a numeric matrix with column names = predictor names, y a numeric vector.
#
# Example (lasso, lambda chosen on the validation block):
#   lasso_model <- list(
#     name = "lasso",
#     tune = function(X, y, h)
#       tune_grid(X, y, h, grid = data.frame(lambda = 10^seq(-4, -1, length = 20)),
#                 fit = lasso_model$fit, predict = lasso_model$predict),
#     fit  = function(X, y, p) glmnet::glmnet(X, y, alpha = 1, lambda = p$lambda),
#     predict = function(fit, X) as.numeric(predict(fit, X)))
# =============================================================================

# ---- Validation split (shared by every tune function) -----------------------
# Split n chronologically ordered training rows into
#   [ fit block ][ gap of h-1 rows ][ validation: last n_val rows ]
# The gap keeps fit-block targets (months s+1..s+h) from overlapping
# validation targets.
val_split <- function(n, h, n_val = VAL_MONTHS) {
  stopifnot(n - n_val - (h - 1) > 0)
  list(fit = seq_len(n - n_val - (h - 1)), val = (n - n_val + 1):n)
}

# Generic grid search on the validation block, for the other models (lasso,
# ridge, RF, PLS, ...): fit each row of `grid` on the fit block, score MSE on
# the validation block, return the best row (with its val_mse).
tune_grid <- function(X, y, h, grid, fit, predict, n_val = VAL_MONTHS) {
  s <- val_split(nrow(X), h, n_val)
  grid <- as.data.frame(grid)
  mse <- vapply(seq_len(nrow(grid)), function(j) {
    m <- fit(X[s$fit, , drop = FALSE], y[s$fit], grid[j, , drop = FALSE])
    mean((y[s$val] - predict(m, X[s$val, , drop = FALSE]))^2)
  }, numeric(1))
  cbind(grid[which.min(mse), , drop = FALSE], val_mse = min(mse))
}

# ---- Core loop: one model x one predictor set x one horizon -----------------
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

# ---- Run a model over all predictor sets and horizons -----------------------
# sets: named list like prep$sets (each element has $vars)
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
  insp_names <- unique(unlist(lapply(runs, \(r) names(r$inspect))))
  list(fc      = bind_rows(lapply(runs, `[[`, "fc")),
       tuning  = bind_rows(lapply(runs, `[[`, "tuning")),
       inspect = setNames(lapply(insp_names, \(nm)
                   bind_rows(lapply(runs, \(r) r$inspect[[nm]]))), insp_names),
       fits    = bind_rows(lapply(runs, `[[`, "fits")))
}

# ---- Save outputs: forecasts (.rds) + tuning / inspect logs (.csv) ----------
save_poos <- function(res, name) {
  save_forecasts(res$fc, name)
  dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
  if (nrow(res$tuning))
    write.csv(res$tuning, file.path(TAB_DIR, paste0(name, "_tuning.csv")), row.names = FALSE)
  for (nm in names(res$inspect)) if (nrow(res$inspect[[nm]]))
    write.csv(res$inspect[[nm]], file.path(TAB_DIR, paste0(name, "_", nm, ".csv")),
              row.names = FALSE)
}
