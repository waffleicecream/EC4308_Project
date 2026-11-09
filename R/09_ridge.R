# =============================================================================
# 09_ridge.R  -  Ridge regression, evaluated with the shared POOS engine
#                (poos.R)
#
# Ridge = OLS with an L2 penalty: minimise (1/2n) RSS + lambda/2 * sum(beta^2).
# glmnet() with alpha = 0 (glmnet standardizes the predictors internally, so
# the penalty treats them equally; coefficients are returned on the original
# scale). Ridge shrinks every coefficient towards 0 but never sets one exactly
# to 0, so it keeps all predictors - it handles the exactly collinear pairs
# (de = dp - ep, tms = lty - tbl) without any special treatment.
#
# Why lambda is chosen on the validation block and NOT by BIC (as 06_lasso.R
# does): ic.glmnet() counts df = number of nonzero coefficients. For ridge
# that is always all p predictors, whatever lambda is, so BIC would simply
# pick the smallest lambda, i.e. (almost) OLS. Instead ridge uses the same
# validation scheme as XGBoost (val_split() in poos.R):
#
#   TUNE (every RETUNE_EVERY = 12 origins):
#     split the training window:  [ fit block ][ gap h-1 ][ validation: last 240 months ]
#     fit the whole ridge path (LAMBDA_GRID) on the fit block, compute the
#     validation MSE of each lambda, keep the lambda with the lowest MSE
#
#   FIT (every origin): refit on the whole training window with that lambda.
#
#   FORECAST (every origin): predict y_h (log premium over t+1..t+h) from x_t.
#
# LAMBDA_GRID runs from ~OLS (1e-6) to full shrinkage (1e2, where every
# coefficient is ~0 and the forecast is the window mean of y, i.e. the
# prevailing-mean benchmark). The tuning log flags both ends of the grid:
#   full_shrink = TRUE : the largest lambda won - validation says the
#                        predictors do not help, so ridge forecasts (almost
#                        exactly) the window mean. A legitimate outcome;
#                        a larger lambda would change nothing.
#   at_ols_end  = TRUE : the smallest lambda won - the grid should be extended
#                        downwards (a warning is printed).
#
# Outputs: output/forecasts/ridge.rds          forecasts (model, set, h, date, y, yhat)
#          output/tables/ridge_tuning.csv      chosen lambda at each tuning date
#          output/tables/ridge_coef.csv        coefficients at every origin
#                                              (coef = original scale,
#                                               std_coef = coef x sd(x) in the window)
#          output/tables/ridge_coef_summary.csv average standardized coefficient
#          output/tables/ridge_eval.csv        test-period evaluation vs benchmark
# Picked up automatically by 04_trading.R.
#
# Run 01_data_prep.R and 02_benchmark.R first.
#
# Packages (run once):  install.packages("glmnet")
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")   # new_coef_log(), log_coef()
suppressPackageStartupMessages(library(glmnet))

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

# ---- Hyperparameter grid ----------------------------------------------------
# Decreasing order, as glmnet expects for a path (warm starts).
LAMBDA_GRID <- 10^seq(2, -6, by = -0.1)             # 81 values

ridge_path <- function(X, y)
  glmnet(X, y, alpha = 0, lambda = LAMBDA_GRID, standardize = TRUE)

# ---- Model definition (POOS interface: tune / fit / predict) ----------------
ridge_log <- new_coef_log()   # coefficients of every window (one per origin)

ridge_model <- list(
  name = "ridge",

  # One glmnet path on the fit block scores every lambda at once, so this uses
  # its own loop rather than tune_grid() (same val_split(), same criterion:
  # validation MSE). It must NOT call ridge_model$fit, so that the coefficient
  # log stays one entry per origin, aligned with res$fits.
  tune = function(X, y, h) {
    s    <- val_split(nrow(X), h)
    path <- ridge_path(X[s$fit, , drop = FALSE], y[s$fit])
    pred <- predict(path, X[s$val, , drop = FALSE], s = LAMBDA_GRID)
    mse  <- colMeans((y[s$val] - pred)^2)
    j    <- which.min(mse)
    data.frame(lambda       = LAMBDA_GRID[j],
               log10_lambda = log10(LAMBDA_GRID[j]),
               val_mse      = mse[j],
               full_shrink  = j == 1,                    # largest lambda: forecast = window mean
               at_ols_end   = j == length(LAMBDA_GRID))  # smallest lambda: grid may be too narrow
  },

  # Fit the whole path (more stable than a single lambda in glmnet) and keep
  # the tuned lambda for prediction; it is on the grid, so no interpolation.
  fit = function(X, y, params) {
    path <- ridge_path(X, y)
    b    <- as.matrix(coef(path, s = params$lambda))[, 1]
    sdx  <- apply(X, 2, sd)
    log_coef(ridge_log, data.frame(var = names(b), coef = unname(b),
                                   std_coef = unname(b * c(NA, sdx[names(b)[-1]]))))
    list(path = path, lambda = params$lambda)
  },

  predict = function(fit, X)
    as.numeric(predict(fit$path, X, s = fit$lambda))   # intercept + X %*% beta
)

# ---- Run ----------------------------------------------------------------------
cat(sprintf("Ridge: lambda tuned every %d origins on a %d-month validation block, grid 1e%d..1e%d\n",
            RETUNE_EVERY, VAL_MONTHS, log10(min(LAMBDA_GRID)), log10(max(LAMBDA_GRID))))
res <- poos_all(ridge_model, d, prep$sets, HORIZONS)
save_poos(res, "ridge")                             # forecasts + ridge_tuning.csv

# ---- Coefficient tables -----------------------------------------------------
stopifnot(length(ridge_log$b) == nrow(res$fits))
coefs <- bind_rows(lapply(seq_along(ridge_log$b), function(k)
  data.frame(res$fits[k, c("model", "set", "h", "origin")], ridge_log$b[[k]],
             row.names = NULL))) |>
  filter(var != "(Intercept)")
write.csv(coefs, file.path(TAB_DIR, "ridge_coef.csv"), row.names = FALSE)

# Average standardized coefficient (effect of a 1-sd move in x on y_h) over
# the test origins, one column per set x horizon; predictors in data order.
coef_summary <- coefs |>
  group_by(var, set, h) |>
  summarise(mean_std_coef = signif(mean(std_coef), 3), .groups = "drop") |>
  pivot_wider(names_from = c(set, h), values_from = mean_std_coef,
              names_sep = "_h", names_sort = TRUE)
coef_summary <- coef_summary[match(unique(coefs$var), coef_summary$var), ]
write.csv(coef_summary, file.path(TAB_DIR, "ridge_coef_summary.csv"), row.names = FALSE)

# ---- Evaluate on the test period vs the prevailing-mean benchmark -----------
ev <- evaluate_all(res$fc, bench)
write.csv(ev, file.path(TAB_DIR, "ridge_eval.csv"), row.names = FALSE)

cat("\n==== ridge: chosen lambda at each tuning date ====\n")
cat("(log10 lambda; ", max(log10(LAMBDA_GRID)),
    " = full shrinkage, i.e. forecast = window mean)\n", sep = "")
print(res$tuning |>
        tidyr::pivot_wider(id_cols = origin, names_from = c(set, h),
                           values_from = log10_lambda, names_sep = "_h"),
      width = Inf)
if (any(res$tuning$at_ols_end))
  warning("smallest lambda chosen in ", sum(res$tuning$at_ols_end),
          " tuning(s) - extend LAMBDA_GRID downwards")
cat("\n==== ridge: evaluation vs prevailing mean ====\n")
print(ev, width = Inf)
cat("\n==== ridge: mean standardized coefficients ====\n")
print(as.data.frame(coef_summary))
