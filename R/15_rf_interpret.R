# =============================================================================
# 15_rf_interpret.R  -  random forest variable importance plots
#
# Averages the permutation importance (% rise in MSE when shuffled) logged at
# the annual tuning dates, per predictor set x horizon, and plots it, for
# single variables and for groups of correlated predictors, under both methods:
#   out-of-bag (03_rf.R) and validation block (14_forest_val_importance.R),
# plus a figure comparing the two.
# Plot code: R/functions/forest_interpret.R.
#
# Outputs: output/figures/importance_oob/rf_importance_<set>.png, rf_group_importance_<set>.png
#          output/figures/importance_val/  (same names)
#          output/figures/importance_compare/rf_importance_compare_<set>.png,
#                                            rf_group_importance_compare_<set>.png
#          output/tables/rf/rf_importance_summary.csv, rf_group_importance_summary.csv,
#                        rf_val_importance_summary.csv, rf_val_group_importance_summary.csv
# Run 03_rf.R and 14_forest_val_importance.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forest_interpret.R")

plot_forest_all("rf")
plot_forest_all("rf_lag")   # lagged inflation (03_rf.R); figures named rf_lag_*
