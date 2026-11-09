# =============================================================================
# 08_pca_lasso.R  -  LASSO with principal components, evaluated with the
#                    shared POOS engine
#
# Two models, following the EC4308 Lecture 4 code and the prof's comments on
# the proposal:
#
#   pca_lasso  : "use PCA to extract principal components and include them
#                together with the other predictors" - as in runlasso() of
#                func-lasso.R, the first N_PC principal components are added to
#                the original predictors, then LASSO with lambda selected by BIC
#                (ic.glmnet) chooses among predictors and PCs together.
#
#   pcr_rlasso : principal components regression with the components chosen
#                by rlasso() on the FULL set of PCs ("the most predictive PCs
#                may be way at the end in terms of variance explained"), then
#                OLS on the selected PCs (post-LASSO), so no CV is needed.
#                Zero-variance PCs (from exactly collinear predictors) are
#                dropped first - see pcr_model below.
#
# PCA is done INSIDE each training window (prcomp on the training X only) and
# the forecast-origin x_t is projected with that window's loadings, so no
# information from after the origin enters the components. The predictors are
# standardized before PCA (correlation-matrix PCA): they are in very different
# units (e.g. svar ~ 0.002 vs dp ~ -3.5), so without scaling the first PCs
# would just be the highest-variance variables. (func-lasso.R only centers, but
# there all FRED-MD series are already transformed to comparable units.)
#
# Outputs (for each model name): output/forecasts/<name>.rds,
#   output/tables/<name>_coef.csv, _selection.csv, _eval.csv
#
# Run 01_data_prep.R and 02_benchmark.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")
suppressPackageStartupMessages({
  library(HDeconometrics)
  library(hdm)
})

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

N_PC <- 4   # number of PCs added to the predictors in pca_lasso (4 as in func-lasso.R)

# PCA on the training window; returns the prcomp object (centers, scales and
# loadings of THIS window), used again to project the forecast-origin x_t
fit_pca <- function(X) prcomp(X, center = TRUE, scale. = TRUE)

# ---- pca_lasso: predictors + first N_PC PCs, LASSO selected by BIC -----------
pca_lasso_log <- new_coef_log()

pca_lasso_model <- list(
  name = "pca_lasso",

  fit = function(X, y, params) {
    pca <- fit_pca(X)
    X2  <- cbind(X, pca$x[, 1:N_PC])          # augment predictors by the first N_PC PCs
    model <- ic.glmnet(X2, y, crit = "bic", alpha = 1)
    log_coef(pca_lasso_log, model$coefficients)
    list(pca = pca, model = model)
  },

  predict = function(fit, X) {
    pcs <- predict(fit$pca, X)[, 1:N_PC, drop = FALSE]   # x_t projected on this window's loadings
    as.numeric(predict(fit$model, cbind(X, pcs)))
  }
)

# ---- pcr_rlasso: all PCs, selected by rlasso, OLS on the selected ones -------
pcr_log <- new_coef_log()

pcr_model <- list(
  name = "pcr_rlasso",

  fit = function(X, y, params) {
    pca  <- fit_pca(X)
    # Drop components with (numerically) zero variance. They exist because two
    # predictors are exact linear combinations of others (de = dp - ep,
    # tms = lty - tbl), so the last 2 PCs are pure rounding noise (sd ~ 1e-16).
    # rlasso rescales every regressor to unit variance, which would blow that
    # noise up into "predictors" it can select.
    keep <- pca$sdev > 1e-8 * pca$sdev[1]
    Z    <- pca$x[, keep, drop = FALSE]
    model <- rlasso(Z, y, post = TRUE)        # all remaining PCs
    log_coef(pcr_log, rlasso_coef(model, colnames(Z)))
    list(pca = pca, keep = keep, model = model)
  },

  predict = function(fit, X)
    rlasso_predict(fit$model, predict(fit$pca, X)[, fit$keep, drop = FALSE])
)

# ---- Run, save, evaluate vs the prevailing-mean benchmark -------------------
cat(sprintf("pca_lasso: predictors + first %d PCs, LASSO (BIC)\n", N_PC))
pl  <- run_lasso_type(pca_lasso_model, pca_lasso_log, d, prep$sets, bench)
cat("pcr_rlasso: all PCs, selected by rlasso, post-LASSO OLS\n")
pcr <- run_lasso_type(pcr_model, pcr_log, d, prep$sets, bench)
