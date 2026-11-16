# =============================================================================
# 03_bag.R  -  bagging, evaluated with the shared POOS engine (poos.R)
#
# Same design as 03_rf.R, except every predictor is available at every split
# (mtry = p), so only nodesize is tuned. Reported separately from RF to show
# whether limiting the predictors at each split helps, and to check for
# dominant predictors. Slower than RF because each split considers all of them.
#
# Outputs: output/forecasts/bag.rds                forecasts (model, set, h, date, y, yhat)
#          output/tables/bag/bag_tuning.csv            chosen nodesize at each tune
#          output/tables/bag/bag_importance.csv        one-variable importance at each tune
#          output/tables/bag/bag_group_importance.csv  group importance at each tune
#          output/tables/bag/bag_eval.csv              test-period evaluation vs benchmark
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

# ---- Same model with lagged inflation (bag_lag) -----------------------------
# infl replaced by infl_l1 = infl_{t-1}, because CPI for month t is only
# published in month t+1 (Goyal & Welch 2008, p. 1459). Everything else is
# identical; see lag_infl() in R/functions/forest.R.
# Outputs: output/forecasts/bag_lag.rds, output/tables/bag_lag/bag_lag_*.csv
prep_l <- lag_infl(prep)
res_l  <- run_forest("bag", forest_sets(prep_l$sets), HORIZONS, infl_lag = TRUE)
save_forest_poos(res_l, "bag_lag")

ev_l <- evaluate_all(res_l$fc, bench)
write.csv(ev_l, file.path(model_tab_dir("bag_lag"), "bag_lag_eval.csv"), row.names = FALSE)
print(ev_l, width = Inf)
