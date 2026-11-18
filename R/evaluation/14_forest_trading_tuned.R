# RF and bagging trading results with the most common tuned settings next to
# each row, e.g. "mtry=20 (25/30), nodesize=500 (18/30)".
#
# Output: output/tables/forest_trading_tuned.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")

FOREST_MODELS <- c("rf", "bag")
TUNE_META <- c("model", "set", "h", "origin", "train_start", "train_end",
               "n_train", "val_mse", "tune_h")

mode_count <- function(x) {
  tab <- sort(table(as.character(x)), decreasing = TRUE)
  sprintf("%s (%d/%d)", names(tab)[1], tab[[1]], length(x))
}

tuned <- bind_rows(lapply(FOREST_MODELS, function(m) {
  tu   <- read.csv(file.path(model_tab_dir(m), paste0(m, "_tuning.csv")))
  pars <- setdiff(names(tu), c(TUNE_META, if (m == "bag") "mtry"))   # bagging: mtry = p
  tu |> group_by(model, set, h) |>
    group_modify(\(g, k) tibble(tuned = paste(
      sprintf("%s=%s", pars, sapply(pars, \(p) mode_count(g[[p]]))),
      collapse = ", "))) |>
    ungroup()
}))

summ <- read.csv(file.path(TAB_DIR, "trading_summary.csv")) |>
  filter(model %in% FOREST_MODELS) |>
  left_join(tuned, by = c("model", "set", "h"))

write.csv(summ, file.path(TAB_DIR, "forest_trading_tuned.csv"), row.names = FALSE)
print(summ)
