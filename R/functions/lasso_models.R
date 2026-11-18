# LASSO-type models as constructors, so each parallel worker can rebuild a
# model from its type name (make_lasso_model). Lambda is chosen inside fit(),
# so there is no tune step. fit() stores the coefficients in `log`.
#
#   type        model                                             script
#   lasso_ic    LASSO, lambda by an information criterion         06_lasso.R
#   rlasso      LASSO, plug-in lambda (Belloni et al. 2012)       07_post_lasso.R
#   post_lasso  rlasso selection, then OLS on the selected        07_post_lasso.R
#   pca_lasso   predictors + first n_pc PCs, LASSO by BIC         08_pca_lasso.R
#   pcr_rlasso  all PCs, selected by rlasso, OLS on the selected  08_pca_lasso.R

suppressPackageStartupMessages({
  library(HDeconometrics)   # ic.glmnet()
  library(hdm)              # rlasso()
})

# BIC = n*log(MSE) + (df+1)*log(n); glmnet standardises internally
lasso_ic_model <- function(name, log, ic = "bic") {
  list(
    name = name,
    fit = function(X, y, params) {
      model <- ic.glmnet(X, y, crit = ic, alpha = 1)
      log_coef(log, model$coefficients)
      model
    },
    predict = function(fit, X) as.numeric(predict(fit, X))
  )
}

rlasso_model <- function(name, log, pl) {
  list(
    name = name,
    fit = function(X, y, params) {
      model <- rlasso(X, y, post = pl)
      log_coef(log, rlasso_coef(model, colnames(X)))
      model
    },
    predict = function(fit, X) rlasso_predict(fit, X)
  )
}

# Standardised PCA: the predictors are in very different units
fit_pca <- function(X) prcomp(X, center = TRUE, scale. = TRUE)

pca_lasso_model <- function(name, log, n_pc = 4) {
  list(
    name = name,
    fit = function(X, y, params) {
      pca <- fit_pca(X)
      X2  <- cbind(X, pca$x[, 1:n_pc, drop = FALSE])
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

# Zero-variance PCs are dropped first: de = dp - ep and tms = lty - tbl make
# some components pure rounding noise, which rlasso would rescale to unit
# variance and could select.
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
