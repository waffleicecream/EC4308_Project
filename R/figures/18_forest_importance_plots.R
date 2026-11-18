# RF and bagging importance plots: out-of-bag, validation block, and both
# side by side (plot code: R/functions/forest_interpret.R).
#
# Outputs: output/figures/importance_{oob,val,compare}/
#          output/tables/<model>/<model>_*importance_summary.csv

source("R/00_config.R")
source("R/functions/forest_interpret.R")

for (m in c("rf", "bag")) plot_forest_all(m)

# Short versions for the slides; all values shown are positive
plot_forest_importance("rf",  "importance", "val", top_n = 3, x_lim = c(0, 6))
plot_forest_importance("bag", "importance", "val", top_n = 5, x_lim = c(0, 6))
cat("Saved importance figures to", FIG_DIR, "\n")
