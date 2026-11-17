# =============================================================================
# 18_forest_trading_tuned.R  -  trading results for RF and bagging, with the
#                               tuned settings next to each row
#
# Reads output/tables/trading_summary.csv (from 10_trading.R) and the forest
# tuning logs output/rf_and_bagging/tables/<model>/<model>_tuning.csv (one row per annual
# tuning date, written by save_forest_poos()). Each setting is reported as its
# most common value and how often it was chosen, e.g.
# "mtry=2 (8/10), nodesize=100 (8/10)".
#
# Output: output/rf_and_bagging/tables/forest_trading_tuned.csv
# Run 03_rf.R / 03_bag.R and 10_trading.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forest_paths.R")   # FOREST_OUT

FOREST_MODELS <- c("rf", "bag")

# Columns of the tuning log that are not tuned settings
TUNE_META <- c("model", "set", "h", "origin", "train_start", "train_end",
               "n_train", "val_mse", "tune_h")
# Logged but fixed by design, not tuned (bagging: mtry = p always)
NOT_TUNED <- c("mtry")
fixed_cols <- function(model) if (startsWith(model, "bag")) NOT_TUNED else character()

mode_count <- function(x) {
  tab <- sort(table(as.character(x)), decreasing = TRUE)
  sprintf("%s (%d/%d)", names(tab)[1], tab[[1]], length(x))
}

tune_files <- file.path(FOREST_OUT, "tables", FOREST_MODELS, paste0(FOREST_MODELS, "_tuning.csv"))
tune_files <- tune_files[file.exists(tune_files)]
stopifnot(length(tune_files) > 0)

tuned <- bind_rows(lapply(tune_files, function(f) {
  tu   <- read.csv(f)
  pars <- setdiff(names(tu), c(TUNE_META, fixed_cols(tu$model[1])))
  tu |> group_by(model, set, h) |>
    group_modify(\(g, k) tibble(tuned = paste(
      sprintf("%s=%s", pars, sapply(pars, \(p) mode_count(g[[p]]))),
      collapse = ", "))) |>
    ungroup()
}))

summ <- read.csv(file.path(TAB_DIR, "trading_summary.csv")) |>
  filter(model %in% FOREST_MODELS) |>
  left_join(tuned, by = c("model", "set", "h"))

write.csv(summ, file.path(FOREST_OUT, "tables", "forest_trading_tuned.csv"), row.names = FALSE)
print(summ)
