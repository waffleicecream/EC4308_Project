# XGBoost (EC4308 Lecture 3.2). Every 12 origins, each depth in 1..5 is
# trained on the fit block with early stopping on the validation block, and
# the depth and number of rounds with the lowest validation RMSE are kept.
# The model is refit on the whole window at every origin. At each tuning date
# the refit model's gain importance and partial dependence are logged.
#
# Slow (hours). Outputs: output/forecasts/xgb.rds
#                        output/tables/xgb/xgb_tuning.csv, xgb_importance.csv,
#                                          xgb_pdp.csv, xgb_eval.csv
# Plots: R/figures/19_xgb_interpret.R

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
suppressPackageStartupMessages(library(xgboost))

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

# Fixed: a small learning rate (lecture: lambda <= 0.01, not tuned) and
# stochastic boosting on half the rows. Tuned: depth and number of trees.
ETA        <- 0.01
SUBSAMPLE  <- 0.5
GRID       <- data.frame(max_depth = 1:5)
MAX_ROUNDS <- 5000
ES_ROUNDS  <- 200      # patient, since each tree changes little at eta = 0.01

xgb_params <- function(g, base) {
  list(objective        = "reg:squarederror",
       eta              = ETA,
       max_depth        = g$max_depth,
       subsample        = SUBSAMPLE,
       base_score       = base,          # f0 = training mean of y
       min_child_weight = 1, colsample_bytree = 1, lambda = 1, gamma = 0,
       nthread = 1, seed = SEED)
}

# Partial-dependence grid: 25 points from the 2.5th to the 97.5th percentile
# (0/1 for the technical signals), the same at every date so curves average.
PDP_GRID <- lapply(setNames(nm = prep$sets$ext$vars), function(v) {
  x <- d[[v]]
  if (length(unique(na.omit(x))) <= 2) sort(unique(na.omit(x)))
  else unique(quantile(x, seq(0.025, 0.975, length.out = 25), na.rm = TRUE, names = FALSE))
})

xgb_model <- list(
  name = "xgb",

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

  inspect = function(fit, X) {
    # Gain: share of the total loss reduction from splits on each variable
    imp  <- tryCatch(xgb.importance(model = fit), error = \(e) NULL)
    gain <- setNames(rep(0, ncol(X)), colnames(X))
    if (!is.null(imp)) gain[imp$Feature] <- imp$Gain

    # Partial dependence: set variable v to each grid value for every
    # training row, predict, and average
    n <- nrow(X)
    pdp <- lapply(colnames(X), function(v) {
      g  <- PDP_GRID[[v]]
      Xg <- X[rep(seq_len(n), times = length(g)), , drop = FALSE]
      Xg[, v] <- rep(g, each = n)
      p  <- matrix(predict(fit, Xg), nrow = n)
      data.frame(feature = v, x = g, yhat = colMeans(p))
    })

    list(importance = data.frame(feature = names(gain), gain = unname(gain)),
         pdp        = bind_rows(pdp))
  }
)

cat(sprintf("XGBoost: eta = %g, subsample = %g, depth grid = %s\n",
            ETA, SUBSAMPLE, paste(GRID$max_depth, collapse = ",")))
set.seed(SEED)
res <- poos_all(xgb_model, d, prep$sets, HORIZONS)
save_poos(res, "xgb")
print(save_eval(res, "xgb", bench), width = Inf)
