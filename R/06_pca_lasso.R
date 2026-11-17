# =============================================================================
# 06_pca_lasso.R  -  LASSO with principal components, evaluated with the
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
#                dropped first.
#
# PCA is done INSIDE each training window (prcomp on the training X only) and
# the forecast-origin x_t is projected with that window's loadings, so no
# information from after the origin enters the components. The predictors are
# standardized before PCA (correlation-matrix PCA): they are in very different
# units (e.g. svar ~ 0.002 vs dp ~ -3.5), so without scaling the first PCs
# would just be the highest-variance variables. (func-lasso.R only centers, but
# there all FRED-MD series are already transformed to comparable units.)
#
# Model code: pca_lasso_model() and pcr_rlasso_model() in
# R/functions/lasso_models.R. The 6 set x horizon runs go in parallel.
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
source("R/functions/lasso_models.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

N_PC <- 4   # number of PCs added to the predictors in pca_lasso (4 as in func-lasso.R)

cat(sprintf("pca_lasso: predictors + first %d PCs, LASSO (BIC)\n", N_PC))
pl  <- run_lasso_type("pca_lasso", prep$sets, bench, args = list(n_pc = N_PC))
cat("pcr_rlasso: all PCs, selected by rlasso, post-LASSO OLS\n")
pcr <- run_lasso_type("pcr_rlasso", prep$sets, bench)
