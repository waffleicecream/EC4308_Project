# Rigorous LASSO and post-LASSO (Belloni, Chernozhukov and Hansen 2012), as in
# the Lecture 4 code func-rlasso.R. hdm::rlasso() sets the penalty by the
# plug-in rule, so nothing is tuned.
#   rlasso     : the rlasso estimates
#   post_lasso : rlasso selects the predictors, then OLS on the selected ones
# The plug-in penalty assumes errors independent over time; for h = 3, 12 the
# targets overlap, so it may be too low.
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

run_lasso_type("rlasso", prep$sets, bench)
run_lasso_type("post_lasso", prep$sets, bench)
