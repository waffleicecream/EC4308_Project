# =============================================================================
# 05_post_lasso.R  -  rigorous LASSO and post-LASSO (Belloni, Chernozhukov and
#                     Hansen 2012), evaluated with the shared POOS engine
#
# Follows the EC4308 Lecture 4 code (func-rlasso.R): rlasso() from the hdm
# package picks the penalty by the theory-based plug-in rule instead of an
# information criterion or cross-validation (default settings:
# heteroskedasticity-robust penalty loadings). No tune() step is needed.
#
# Two models:
#   rlasso     : the rlasso estimates themselves (post = FALSE)
#   post_lasso : post-LASSO (post = TRUE) - rlasso only SELECTS the
#                predictors, then OLS is re-run on the selected ones, which
#                removes the LASSO shrinkage bias of the nonzero coefficients
#
# The plug-in penalty is usually larger than the BIC one, so these models
# tend to select fewer predictors than lasso_bic (04_lasso.R).
#
# NB: rlasso's penalty assumes errors that are independent over time. For
# h = 3, 12 the targets overlap (consecutive h-month returns share h-1 months),
# so the effective sample is smaller than n and the penalty may be too low.
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
suppressPackageStartupMessages(library(hdm))

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

# ---- Model definition (POOS interface: fit / predict) -----------------------
# pl = TRUE for post-LASSO, FALSE for the rlasso estimates (as in func-rlasso.R)
make_rlasso_model <- function(name, pl, log) {
  list(
    name = name,
    fit = function(X, y, params) {
      model <- rlasso(X, y, post = pl)   # default: intercept, heteroskedastic penalty
      log_coef(log, rlasso_coef(model, colnames(X)))
      model
    },
    predict = function(fit, X) rlasso_predict(fit, X)   # see lasso_utils.R: not predict()
  )
}

rlasso_log <- new_coef_log()
post_log   <- new_coef_log()
rlasso_model <- make_rlasso_model("rlasso", pl = FALSE, log = rlasso_log)
post_model   <- make_rlasso_model("post_lasso", pl = TRUE, log = post_log)

# ---- Run, save, evaluate vs the prevailing-mean benchmark -------------------
cat("rlasso: plug-in penalty (Belloni et al. 2012) at every origin\n")
rl   <- run_lasso_type(rlasso_model, rlasso_log, d, prep$sets, bench)
post <- run_lasso_type(post_model, post_log, d, prep$sets, bench)
