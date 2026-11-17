# =============================================================================
# lasso_models.R  -  the LASSO-type models, as constructors
#
# Each constructor returns a model in the POOS interface (fit / predict, no
# tune: lambda is chosen inside fit) whose fit() stores its coefficients in
# `log` (see new_coef_log() in lasso_utils.R).
#
# They live here, not in the scripts, so that the parallel runner
# (run_lasso_type() in lasso_utils.R) can rebuild a model inside each worker
# process from its type name alone. Sourced by 04_lasso.R, 05_post_lasso.R,
# 06_pca_lasso.R and by every worker.
#
#   type        model                                              script
#   lasso_ic    LASSO, lambda by an information criterion          04_lasso.R
#   rlasso      LASSO, plug-in lambda (Belloni et al. 2012)        05_post_lasso.R
#   post_lasso  rlasso selection, then OLS on the selected         05_post_lasso.R
#   pca_lasso   predictors + first n_pc PCs, LASSO by BIC          06_pca_lasso.R
#   pcr_rlasso  all PCs, selected by rlasso, OLS on the selected   06_pca_lasso.R
# =============================================================================

suppressPackageStartupMessages({
  library(HDeconometrics)   # ic.glmnet() - as in func-lasso.R
  library(hdm)              # rlasso()    - as in func-rlasso.R
})

# ---- LASSO with lambda chosen by an information criterion -------------------
# ic.glmnet() fits the whole LASSO path (glmnet standardizes the predictors
# internally) and keeps the lambda with the lowest criterion, e.g.
# BIC = n*log(MSE) + (df+1)*log(n). ic = "bic", "aic" or "aicc".
lasso_ic_model <- function(name, log, ic = "bic") {
  list(
    name = name,
    fit = function(X, y, params) {
      model <- ic.glmnet(X, y, crit = ic, alpha = 1)   # alpha = 1 -> LASSO
      log_coef(log, model$coefficients)
      model
    },
    predict = function(fit, X) as.numeric(predict(fit, X))   # intercept + X %*% beta
  )
}

# ---- rlasso / post-LASSO (plug-in penalty) ----------------------------------
# pl = TRUE for post-LASSO, FALSE for the rlasso estimates (as in func-rlasso.R).
# Default settings: intercept, heteroskedasticity-robust penalty loadings.
rlasso_model <- function(name, log, pl) {
  list(
    name = name,
    fit = function(X, y, params) {
      model <- rlasso(X, y, post = pl)
      log_coef(log, rlasso_coef(model, colnames(X)))
      model
    },
    predict = function(fit, X) rlasso_predict(fit, X)   # see lasso_utils.R: not predict()
  )
}

# ---- PCA helpers ------------------------------------------------------------
# PCA on the training window only (standardized predictors: they are in very
# different units), so x_t is projected with THIS window's centers, scales
# and loadings - no information from after the origin enters the components.
fit_pca <- function(X) prcomp(X, center = TRUE, scale. = TRUE)

# ---- pca_lasso: predictors + first n_pc PCs, LASSO selected by BIC -----------
# As in runlasso() of func-lasso.R, which adds the first 4 PCs.
pca_lasso_model <- function(name, log, n_pc = 4) {
  list(
    name = name,
    fit = function(X, y, params) {
      pca <- fit_pca(X)
      X2  <- cbind(X, pca$x[, 1:n_pc, drop = FALSE])   # augment by the first n_pc PCs
      model <- ic.glmnet(X2, y, crit = "bic", alpha = 1)
      log_coef(log, model$coefficients)
      list(pca = pca, model = model)
    },
    predict = function(fit, X) {
      pcs <- predict(fit$pca, X)[, 1:n_pc, drop = FALSE]
      as.numeric(predict(fit$model, cbind(X, pcs)))
    }
  )
}

# ---- pcr_rlasso: all PCs, selected by rlasso, OLS on the selected ones -------
# Components with (numerically) zero variance are dropped first. They exist
# because predictors are exact linear combinations of others (de = dp - ep,
# tms = lty - tbl, at every lag), so those PCs are pure rounding noise
# (sd ~ 1e-16); rlasso rescales every regressor to unit variance, which would
# blow that noise up into "predictors" it can select.
pcr_rlasso_model <- function(name, log) {
  list(
    name = name,
    fit = function(X, y, params) {
      pca  <- fit_pca(X)
      keep <- pca$sdev > 1e-8 * pca$sdev[1]
      Z    <- pca$x[, keep, drop = FALSE]
      model <- rlasso(Z, y, post = TRUE)
      log_coef(log, rlasso_coef(model, colnames(Z)))
      list(pca = pca, keep = keep, model = model)
    },
    predict = function(fit, X)
      rlasso_predict(fit$model, predict(fit$pca, X)[, fit$keep, drop = FALSE])
  )
}

# ---- Build a model from its type name (used by every worker) ----------------
# args: named list of extra settings, e.g. list(ic = "bic") or list(n_pc = 8)
make_lasso_model <- function(type, name, log, args = list()) {
  switch(type,
    lasso_ic   = lasso_ic_model(name, log, ic = if (is.null(args$ic)) "bic" else args$ic),
    rlasso     = rlasso_model(name, log, pl = FALSE),
    post_lasso = rlasso_model(name, log, pl = TRUE),
    pca_lasso  = pca_lasso_model(name, log, n_pc = if (is.null(args$n_pc)) 4 else args$n_pc),
    pcr_rlasso = pcr_rlasso_model(name, log),
    stop("unknown LASSO model type: ", type)
  )
}
