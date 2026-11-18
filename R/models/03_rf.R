# Random forest. mtry in {p/6, p/3, p/2} x nodesize in NODESIZES, tuned every
# 12 origins on the validation block; refit at every origin.
# Model code: R/functions/forest.R.
#
# From scratch this takes hours. Finished jobs are saved in output/checkpoints/
# and reused, so with the saved checkpoints the script finishes in seconds.
#
# Outputs: output/forecasts/rf.rds
#          output/tables/rf/rf_tuning.csv, rf_importance.csv,
#                           rf_group_importance.csv, rf_eval.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/forest.R")

prep  <- readRDS(PROC_FILE)
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

res <- run_forest("rf", prep$sets, HORIZONS)
save_poos(res, "rf")
print(save_eval(res, "rf", bench), width = Inf)
