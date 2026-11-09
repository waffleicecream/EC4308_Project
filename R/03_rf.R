# =============================================================================
# 03_rf.R  -  random forest, evaluated with the shared POOS engine (poos.R)
#
# For each predictor set (gw, ext) and horizon h (1, 3, 12), poos() walks through
# the test origins with a rolling window:
#
#   TUNE (every RETUNE_EVERY = 12 origins): fit each mtry x nodesize pair on the
#     fit block, score it on the validation block (last 240 months of the
#     training window), keep the best
#     + log one-variable and group importance at that date
#   FIT (every origin): refit on the whole training window with those settings
#   FORECAST (every origin): predict y_h from x_t
#
# Run time: hours. Runs in parallel, and a stopped run resumes where it left off
# (output/forest_progress.log shows progress). Model code: R/functions/forest.R.
#
# Outputs: output/forecasts/rf.rds                forecasts (model, set, h, date, y, yhat)
#          output/tables/rf/rf_tuning.csv            chosen settings at each tune
#          output/tables/rf/rf_importance.csv        one-variable importance at each tune
#          output/tables/rf/rf_group_importance.csv  group importance at each tune
#          output/tables/rf/rf_eval.csv              test-period evaluation vs benchmark
# Run 01_data_prep.R and 02_benchmark.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/forest.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

res <- run_forest("rf", prep$sets, HORIZONS)
save_forest_poos(res, "rf")

ev <- evaluate_all(res$fc, bench)
write.csv(ev, file.path(model_tab_dir("rf"), "rf_eval.csv"), row.names = FALSE)
print(ev, width = Inf)

# ---- Same model with lagged inflation (rf_lag) ------------------------------
# infl replaced by infl_l1 = infl_{t-1}, because CPI for month t is only
# published in month t+1 (Goyal & Welch 2008, p. 1459). Everything else is
# identical; see lag_infl() in R/functions/forest.R.
# Outputs: output/forecasts/rf_lag.rds, output/tables/rf_lag/rf_lag_*.csv
prep_l <- lag_infl(prep)
res_l  <- run_forest("rf", prep_l$sets, HORIZONS, infl_lag = TRUE)
save_forest_poos(res_l, "rf_lag")

ev_l <- evaluate_all(res_l$fc, bench)
write.csv(ev_l, file.path(model_tab_dir("rf_lag"), "rf_lag_eval.csv"), row.names = FALSE)
print(ev_l, width = Inf)
