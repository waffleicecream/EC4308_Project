# =============================================================================
# 04_lasso.R  -  LASSO with lambda selected by BIC, evaluated with the shared
#                POOS engine (poos.R)
#
# Follows the EC4308 Lecture 4 code (func-lasso.R, Medeiros et al. 2019):
# lambda is chosen by an information criterion via ic.glmnet() from the
# HDeconometrics package, NOT by a validation block. So the model has no
# tune() step - BIC re-selects lambda inside every window, at every origin.
#
# For each predictor set (gw, ext) and horizon h (1, 3, 12), poos() walks
# through the test origins (FIRST_ORIGIN onward) with the rolling window:
#
#   FIT (every origin): ic.glmnet() fits the whole LASSO path on the training
#     window (glmnet standardizes the predictors internally) and keeps the
#     lambda with the lowest BIC = n*log(MSE) + (df+1)*log(n).
#
#   FORECAST (every origin): predict y_h (log premium over t+1..t+h) from x_t.
#
# Model code: lasso_ic_model() in R/functions/lasso_models.R. The 6 set x
# horizon runs go in parallel (run_lasso_type() in R/functions/lasso_utils.R).
#
# Outputs: output/forecasts/lasso_bic.rds        forecasts (model, set, h, date, y, yhat)
#          output/tables/lasso_bic_coef.csv      coefficients at every origin
#          output/tables/lasso_bic_selection.csv % of origins each predictor is selected
#          output/tables/lasso_bic_eval.csv      test-period evaluation vs benchmark
#
# Run 01_data_prep.R and 02_benchmark.R first.
#
# Packages (run once):
#   install.packages(c("glmnet","remotes"))
#   remotes::install_github("gabrielrvsc/HDeconometrics")
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")
source("R/functions/lasso_models.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

IC <- "bic"   # information criterion for lambda: "bic", "aic" or "aicc" (as in func-lasso.R)

cat(sprintf("LASSO: lambda selected by %s at every origin\n", toupper(IC)))
lasso <- run_lasso_type("lasso_ic", prep$sets, bench,
                        name = paste0("lasso_", IC), args = list(ic = IC))
