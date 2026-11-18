# Bagging: the random forest with mtry = p, so only nodesize is tuned.
# Slower than RF because every split considers all predictors.
# Model code: R/functions/forest.R. Checkpoints as in 03_rf.R.
#
# Outputs: output/forecasts/bag.rds
#          output/tables/bag/bag_tuning.csv, bag_importance.csv,
#                            bag_group_importance.csv, bag_eval.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/forest.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

res <- run_forest("bag", prep$sets, HORIZONS)
save_poos(res, "bag")
print(save_eval(res, "bag", bench), width = Inf)
