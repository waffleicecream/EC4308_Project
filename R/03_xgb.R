# =============================================================================
# 03_xgb.R  -  XGBoost, evaluated with the shared POOS engine (poos.R)
#
# For each predictor set (gw, ext) and horizon h (1, 3, 12), poos() walks
# through the test origins (FIRST_ORIGIN onward) with a rolling window:
#
#   TUNE (every RETUNE_EVERY = 12 origins):
#     split the training window:  [ fit block ][ gap h-1 ][ validation: last 240 months ]
#     for each depth in GRID:
#       train on the fit block, early-stop on validation RMSE
#     keep the depth (and number of rounds) with lowest validation RMSE
#     + log variable importance and partial dependence of the refitted model
#
#   FIT (every origin): refit on the whole training window with the chosen
#     depth and number of rounds.
#
#   FORECAST (every origin): predict y_h from x_t with the current fit.
#
# Outputs: output/forecasts/xgb.rds          forecasts (model, set, h, date, y, yhat)
#          output/tables/xgb_tuning.csv      chosen hyperparameters at each tune
#          output/tables/xgb_importance.csv  variable importance at each tune
#          output/tables/xgb_pdp.csv         partial dependence at each tune
#          output/tables/xgb_eval.csv        test-period evaluation vs benchmark
# Plots: run 05_xgb_interpret.R
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
suppressPackageStartupMessages(library(xgboost))

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

# ---- Hyperparameters (following EC4308 Lecture 3.2, slides 24-28) -----------
# Fixed (not tuned):
#   eta       = shrinkage lambda: small and fixed - boosting works best when
#               learning is slow (lecture: lambda <= 0.01, "don't tune")
#   subsample = 0.5: stochastic gradient boosting, each tree grown on half
#               the training rows (Friedman 1999)
# Tuned on the validation block:
#   max_depth = tree depth d in 1..5
#   nrounds   = number of trees M, by early stopping (lecture: large, by CV)
# Everything else is left at XGBoost defaults (listed in xgb_params()).
ETA        <- 0.01
SUBSAMPLE  <- 0.5
GRID       <- data.frame(max_depth = 1:5)
MAX_ROUNDS <- 5000   # upper bound on M; early stopping picks the actual M
ES_ROUNDS  <- 200    # stop if validation RMSE has not improved for 200 rounds
                     # (patient, because with eta = 0.01 each tree changes little)

xgb_params <- function(g, base) {
  list(objective        = "reg:squarederror",   # L2 boosting (squared loss)
       eta              = ETA,
       max_depth        = g$max_depth,
       subsample        = SUBSAMPLE,
       base_score       = base,   # f0(x) = training mean of y (lecture slide 27)
       # XGBoost defaults, stated explicitly so the model is fully documented:
       min_child_weight = 1, colsample_bytree = 1, lambda = 1, gamma = 0,
       nthread = 1, seed = SEED)
}

# ---- Model definition (POOS interface: tune / fit / predict / inspect) ------
xgb_model <- list(
  name = "xgb",

  # Early stopping picks nrounds, so XGBoost uses its own tuner rather than
  # tune_grid(); it still uses the shared val_split().
  tune = function(X, y, h) {
    s    <- val_split(nrow(X), h)
    dfit <- xgb.DMatrix(X[s$fit, , drop = FALSE], label = y[s$fit])
    dval <- xgb.DMatrix(X[s$val, , drop = FALSE], label = y[s$val])
    res <- lapply(seq_len(nrow(GRID)), function(j) {
      g <- GRID[j, , drop = FALSE]
      m <- xgb.train(params = xgb_params(g, mean(y[s$fit])), data = dfit,
                     nrounds = MAX_ROUNDS, evals = list(val = dval),
                     early_stopping_rounds = ES_ROUNDS, verbose = 0)
      es <- attributes(m)$early_stop
      data.frame(g, nrounds = max(1, es$best_iteration),
                 val_rmse = unname(es$best_score))
    })
    res <- do.call(rbind, res)
    res[which.min(res$val_rmse), ]
  },

  fit = function(X, y, p)
    xgb.train(params = xgb_params(p, mean(y)), data = xgb.DMatrix(X, label = y),
              nrounds = p$nrounds, verbose = 0),

  predict = function(fit, X) predict(fit, X),

  # Logged at each tuning date, for the model refitted on that date's window.
  inspect = function(fit, X) {
    # Relative importance (lecture slides 38-39): XGBoost "gain" = share of the
    # total improvement in squared-error loss from splits on each variable,
    # i.e. the squared relevance I_l^2 summed over all trees. Variables never
    # split on get 0.
    imp  <- tryCatch(xgb.importance(model = fit), error = \(e) NULL)
    gain <- setNames(rep(0, ncol(X)), colnames(X))
    if (!is.null(imp)) gain[imp$Feature] <- imp$Gain

    # Partial dependence (lecture slide 41): f_S(v) = (1/N) sum_i f(v, x_iC),
    # i.e. set variable S to v for EVERY training row, keep the other
    # variables at their actual values, predict, and average.
    n <- nrow(X)
    pdp <- lapply(colnames(X), function(v) {
      g  <- PDP_GRID[[v]]
      Xg <- X[rep(seq_len(n), times = length(g)), , drop = FALSE]
      Xg[, v] <- rep(g, each = n)
      p  <- matrix(predict(fit, Xg), nrow = n)      # column j = grid point j
      data.frame(feature = v, x = g, yhat = colMeans(p))
    })

    list(importance = data.frame(feature = names(gain), gain = unname(gain)),
         pdp        = bind_rows(pdp))
  }
)

# Partial-dependence grid per variable, the same at every tuning date so the
# curves can be averaged: 25 points from the 2.5th to the 97.5th percentile of
# the variable over the sample (0/1 for the binary technical indicators).
PDP_GRID <- lapply(setNames(nm = prep$sets$ext$vars), function(v) {
  x <- d[[v]]
  if (length(unique(na.omit(x))) <= 2) sort(unique(na.omit(x)))
  else unique(quantile(x, seq(0.025, 0.975, length.out = 25), na.rm = TRUE, names = FALSE))
})

# ---- Run ----------------------------------------------------------------------
cat(sprintf("XGBoost: eta = %g, subsample = %g, depth grid = %s\n",
            ETA, SUBSAMPLE, paste(GRID$max_depth, collapse = ",")))
set.seed(SEED)
res <- poos_all(xgb_model, d, prep$sets, HORIZONS)
save_poos(res, "xgb")

# ---- Evaluate on the test period vs the prevailing-mean benchmark -----------
ev <- evaluate_all(res$fc, bench)
write.csv(ev, file.path(TAB_DIR, "xgb_eval.csv"), row.names = FALSE)
print(ev, width = Inf)
