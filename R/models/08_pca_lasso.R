# LASSO with principal components.
#   pca_lasso  : the first 4 PCs are added to the predictors, then LASSO by BIC
#                (as runlasso() in func-lasso.R)
#   pcr_rlasso : rlasso picks among all PCs, then OLS on the selected ones
# PCA runs inside each training window on standardised predictors, and x_t is
# projected with that window's loadings, so nothing after the origin is used.
#
# Outputs: output/forecasts/<name>.rds and output/tables/<name>/ for each model

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")
source("R/functions/lasso_models.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

run_lasso_type("pca_lasso", prep$sets, bench, args = list(n_pc = 4))
run_lasso_type("pcr_rlasso", prep$sets, bench)
