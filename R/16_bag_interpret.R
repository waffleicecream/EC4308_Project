# =============================================================================
# 16_bag_interpret.R  -  bagging variable importance plots
#
# Averages the permutation importance (% rise in MSE when shuffled) logged at
# the annual tuning dates, per predictor set x horizon, and plots it, for
# single variables and for groups of correlated predictors, under both methods:
#   out-of-bag (03_bag.R) and validation block (14_forest_val_importance.R),
# plus a figure comparing the two.
# Plot code: R/functions/forest_interpret.R.
#
# Outputs: output/figures/importance_oob/bag_importance_<set>.png, bag_group_importance_<set>.png
#          output/figures/importance_val/  (same names)
#          output/figures/importance_compare/bag_importance_compare_<set>.png,
#                                            bag_group_importance_compare_<set>.png
#          output/tables/bag/bag_importance_summary.csv, bag_group_importance_summary.csv,
#                        bag_val_importance_summary.csv, bag_val_group_importance_summary.csv
# Run 03_bag.R and 14_forest_val_importance.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forest_interpret.R")

plot_forest_all("bag")
plot_forest_all("bag_lag")  # lagged inflation (03_bag.R); figures named bag_lag_*
