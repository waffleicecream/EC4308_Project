# =============================================================================
# 03_bag.R  -  bagging, evaluated with the shared POOS engine (poos.R)
#
# Same design as 03_rf.R, except every predictor is available at every split
# (mtry = p), so only nodesize is tuned. Reported separately from RF to show
# whether limiting the predictors at each split helps, and to check for
# dominant predictors. Slower than RF because each split considers all of them.
#
# Outputs: output/forecasts/bag.rds                forecasts (model, set, h, date, y, yhat)
#          output/rf_and_bagging/tables/bag/bag_tuning.csv            chosen nodesize at each tune
#          output/rf_and_bagging/tables/bag/bag_importance.csv        one-variable importance at each tune
#          output/rf_and_bagging/tables/bag/bag_group_importance.csv  group importance at each tune
#          output/rf_and_bagging/tables/bag/bag_eval.csv              test-period evaluation vs benchmark
# Run 01_data_prep.R and 02_benchmark.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/forest.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

res <- run_forest("bag", forest_sets(prep$sets), HORIZONS)
save_forest_poos(res, "bag")

ev <- evaluate_all(res$fc, bench)
write.csv(ev, file.path(model_tab_dir("bag"), "bag_eval.csv"), row.names = FALSE)
print(ev, width = Inf)
