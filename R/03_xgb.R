# =============================================================================
# 03_xgb.R  -  XGBoost: tune on validation, forecast on a rolling window
#
# For each predictor set (gw, ext) and horizon h (1, 3, 12), at every test
# origin t (1996-12 onward):
#
#   training window = last WINDOW (180) rows whose h-month target is realised
#                     by t  (train_rows(), drops the last h-1 rows automatically)
#
#   every RETUNE_EVERY (12) origins -> TUNE:
#     split the window:  [ fit block ][ gap h-1 ][ validation: last 36 months ]
#     for each hyperparameter combination in GRID:
#       train on the fit block, early-stop on validation RMSE
#     keep the combination (and number of rounds) with lowest validation RMSE
#
#   every origin -> REFIT on the whole 180-month window with the chosen
#   hyperparameters and number of rounds, then forecast y_h from x_t.
#
# The gap of h-1 rows keeps fit-block targets (months s+1..s+h) from
# overlapping validation targets.
#
# Outputs: output/forecasts/xgb.rds        forecasts (model, set, h, date, y, yhat)
#          output/tables/xgb_tuning.csv    chosen hyperparameters at each retune
#          output/tables/xgb_importance.csv  gain importance at each retune
#          output/tables/xgb_eval.csv      test-period evaluation vs benchmark
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
suppressPackageStartupMessages(library(xgboost))

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

# ---- Hyperparameters --------------------------------------------------------
# Shallow trees and strong regularisation: monthly returns are mostly noise.
GRID <- expand.grid(max_depth        = c(1, 2, 3),
                    eta              = c(0.03, 0.1),
                    min_child_weight = c(5, 20),
                    colsample_bytree = c(0.5, 0.8),
                    subsample        = 0.8)
MAX_ROUNDS <- 1000
ES_ROUNDS  <- 50     # stop if validation RMSE has not improved for 50 rounds

xgb_params <- function(g, base) {
  list(objective = "reg:squarederror",
       eta = g$eta, max_depth = g$max_depth,
       min_child_weight = g$min_child_weight,
       subsample = g$subsample, colsample_bytree = g$colsample_bytree,
       base_score = base,      # start from the training mean, not 0.5
       nthread = 1, seed = SEED)
}

# ---- Tuning on the validation block -----------------------------------------
tune_xgb <- function(X, y, h) {
  n     <- nrow(X)
  v_idx <- (n - XGB_VAL_MONTHS + 1):n
  f_idx <- seq_len(n - XGB_VAL_MONTHS - (h - 1))
  dfit  <- xgb.DMatrix(X[f_idx, , drop = FALSE], label = y[f_idx])
  dval  <- xgb.DMatrix(X[v_idx, , drop = FALSE], label = y[v_idx])

  res <- lapply(seq_len(nrow(GRID)), function(j) {
    g <- GRID[j, ]
    m <- xgb.train(params = xgb_params(g, mean(y[f_idx])), data = dfit,
                   nrounds = MAX_ROUNDS, evals = list(val = dval),
                   early_stopping_rounds = ES_ROUNDS, verbose = 0)
    es <- attributes(m)$early_stop
    data.frame(g, nrounds = max(1, es$best_iteration),
               val_rmse = unname(es$best_score))
  })
  res <- do.call(rbind, res)
  res[which.min(res$val_rmse), ]
}

# ---- Rolling forecasts for one set x horizon ---------------------------------
run_xgb <- function(set, h) {
  vars <- prep$sets[[set]]$vars
  yc   <- target_col(h)
  idx  <- test_origins(d, h)
  yhat <- numeric(length(idx))
  tune_log <- list(); imp_log <- list(); best <- NULL

  for (k in seq_along(idx)) {
    i  <- idx[k]
    tr <- d[train_rows(i, h, WINDOW), c(yc, vars)] |> drop_na()  # ext: early
    X  <- as.matrix(tr[vars]); y <- tr[[yc]]                     # windows shorter

    if ((k - 1) %% RETUNE_EVERY == 0) {
      best <- tune_xgb(X, y, h)
      tune_log[[length(tune_log) + 1]] <-
        data.frame(set = set, h = h, origin = d$date[i], n_train = nrow(X), best)
    }

    fit <- xgb.train(params = xgb_params(best, mean(y)),
                     data = xgb.DMatrix(X, label = y),
                     nrounds = best$nrounds, verbose = 0)
    yhat[k] <- predict(fit, as.matrix(d[i, vars]))

    if ((k - 1) %% RETUNE_EVERY == 0) {
      imp <- tryCatch(xgb.importance(model = fit), error = \(e) NULL)
      if (!is.null(imp) && nrow(imp) > 0)
        imp_log[[length(imp_log) + 1]] <-
          data.frame(set = set, h = h, origin = d$date[i],
                     feature = imp$Feature, gain = imp$Gain)
    }
  }

  cat(sprintf("  done: set=%s h=%d (%d forecasts)\n", set, h, length(idx)))
  list(fc     = make_forecast_tbl(d, idx, h, yhat, model = "xgb", set = set),
       tuning = do.call(rbind, tune_log),
       imp    = do.call(rbind, imp_log))
}

# ---- Run all ------------------------------------------------------------------
set.seed(SEED)
t0 <- Sys.time()
runs <- list()
for (s in c("gw", "ext")) for (h in HORIZONS) runs[[paste(s, h)]] <- run_xgb(s, h)
cat("Elapsed:", format(round(Sys.time() - t0, 1)), "\n")

fc <- bind_rows(lapply(runs, `[[`, "fc"))
save_forecasts(fc, "xgb")
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(bind_rows(lapply(runs, `[[`, "tuning")),
          file.path(TAB_DIR, "xgb_tuning.csv"), row.names = FALSE)
write.csv(bind_rows(lapply(runs, `[[`, "imp")),
          file.path(TAB_DIR, "xgb_importance.csv"), row.names = FALSE)

# ---- Evaluate on the test period vs the prevailing-mean benchmark -----------
ev <- bind_rows(lapply(split(fc, list(fc$set, fc$h), drop = TRUE), function(f) {
  hh <- f$h[1]
  stats <- evaluate_vs_bench(f, bench |> filter(h == hh), hh)
  tibble(set = f$set[1], h = hh, !!!as.list(round(stats, 3)))
})) |> arrange(set, h)
write.csv(ev, file.path(TAB_DIR, "xgb_eval.csv"), row.names = FALSE)
print(ev, width = Inf)
