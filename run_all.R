# Rebuild every result from data/Data2025.xlsx. Run from the project root:
#
#   Rscript run_all.R                     reproduces the committed results (~16 min)
#   Rscript run_all.R --xgb               also re-runs XGBoost (adds hours)
#   Rscript run_all.R --val-importance    also re-runs the RF/bagging
#                                         validation-block importance (~8 min)
#
# Each step runs in a fresh R process and the run stops at the first error.
# RF and bagging reload their saved runs from output/checkpoints/; delete
# those files to re-estimate the forests from scratch (hours).

args    <- commandArgs(trailingOnly = TRUE)
run_xgb <- "--xgb" %in% args
val_imp_files <- c("output/tables/rf/rf_val_importance.csv",
                   "output/tables/bag/bag_val_importance.csv")
run_val <- "--val-importance" %in% args || !all(file.exists(val_imp_files))

steps <- c(
  "R/01_data_prep.R",
  "R/02_benchmark.R",
  "R/models/03_rf.R",
  "R/models/04_bag.R",
  if (run_xgb) "R/models/05_xgb.R",
  "R/models/06_lasso.R",
  "R/models/07_post_lasso.R",
  "R/models/08_pca_lasso.R",
  "R/models/09_ridge.R",
  "R/models/10_kitchen_sink.R",
  "R/evaluation/11_ensemble.R",
  "R/evaluation/12_all_eval.R",
  "R/evaluation/13_trading.R",
  "R/evaluation/14_forest_trading_tuned.R",
  if (run_val) "R/evaluation/15_forest_val_importance.R",
  "R/figures/16_performance_plots.R",
  "R/figures/17_forecast_plots.R",
  "R/figures/18_forest_importance_plots.R",
  if (file.exists("output/tables/xgb/xgb_importance.csv") || run_xgb)
    "R/figures/19_xgb_interpret.R",
  "R/figures/20_corr_plot.R"
)

if (!file.exists("R/00_config.R")) stop("Run this from the project root.", call. = FALSE)
if (!run_xgb && !file.exists("output/forecasts/xgb.rds"))
  stop("output/forecasts/xgb.rds is missing. Run with --xgb.", call. = FALSE)
if (!run_xgb) cat("Skipping 05_xgb.R; using the saved output/forecasts/xgb.rds.\n")
if (!run_val) cat("Skipping 15_forest_val_importance.R; using the saved importance tables.\n")

rscript <- file.path(R.home("bin"), "Rscript")
t_all   <- Sys.time()
for (s in steps) {
  cat(sprintf("\n==> %s\n", s))
  t0 <- Sys.time()
  status <- system2(rscript, s)
  if (status != 0) stop(s, " failed (exit status ", status, ").", call. = FALSE)
  cat(sprintf("<== %s done in %s\n", s, format(round(Sys.time() - t0, 1))))
}
cat(sprintf("\nAll steps done in %s.\n", format(round(Sys.time() - t_all, 1))))
