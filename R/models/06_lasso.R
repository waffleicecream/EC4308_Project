# LASSO with lambda chosen by BIC inside every window (ic.glmnet() from
# HDeconometrics, as in the Lecture 4 code func-lasso.R). No validation block.
# Model code: R/functions/lasso_models.R.
#
# Outputs: output/forecasts/lasso_bic.rds
#          output/tables/lasso_bic/lasso_bic_coef.csv, _selection.csv, _eval.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")
source("R/functions/lasso_models.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

run_lasso_type("lasso_ic", prep$sets, bench, name = "lasso_bic", args = list(ic = "bic"))
