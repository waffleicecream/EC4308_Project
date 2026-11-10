# =============================================================================
# 13_all_eval.R  -  one table of every model's test-period evaluation
#
# Stacks the per-model *_eval.csv files written by the model scripts and
# 11_ensemble.R. Same columns as those files; one row per model x set x horizon.
# Forest evals live in output/tables/<model>/, so the search is recursive.
# The combined file is excluded on re-run so it is not stacked into itself.
#
# Output: output/tables/all_eval.csv
# Run the model scripts and 11_ensemble.R first.
# =============================================================================

source("R/00_config.R")

OUT <- file.path(TAB_DIR, "all_eval.csv")
COLS <- c("model", "set", "h", "n", "oos_r2", "dm_stat", "dm_p",
          "hit", "hit_bench", "pt_stat", "pt_p", "pct_in_mkt")

# Same order as the other summary scripts, with the inflation-lag checks
# next to the forests they vary.
MODEL_ORDER <- c("ensemble_mean", "rf", "rf_lag", "bag", "bag_lag", "xgb",
                 "lasso_bic", "rlasso", "post_lasso", "pca_lasso", "pcr_rlasso",
                 "ridge")

files <- setdiff(
  list.files(TAB_DIR, "_eval\\.csv$", recursive = TRUE, full.names = TRUE),
  OUT
)
if (!length(files))
  stop("No *_eval.csv files in ", TAB_DIR, ". Run the model scripts first.",
       call. = FALSE)

parts <- lapply(files, function(f) {
  x <- read.csv(f, stringsAsFactors = FALSE)
  missing <- setdiff(COLS, names(x))
  if (length(missing))
    stop(basename(f), " is missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  x[COLS]
})
ev <- bind_rows(parts)

if (anyDuplicated(ev[c("model", "set", "h")]))
  stop("Duplicate model x set x horizon rows across eval files.", call. = FALSE)

# Every model is scored on the same origins, so n and the benchmark hit rate
# must agree within each set and horizon.
check <- ev %>%
  group_by(set, h) %>%
  summarise(n_n = n_distinct(n), n_hit_bench = n_distinct(hit_bench),
            .groups = "drop")
if (any(check$n_n != 1 | check$n_hit_bench != 1))
  stop("Eval files disagree on n or hit_bench within a set and horizon.",
       call. = FALSE)

extra <- setdiff(unique(ev$model), MODEL_ORDER)
ev <- ev %>%
  mutate(model = factor(model, levels = c(MODEL_ORDER, sort(extra))),
         set   = factor(set, levels = c("gw", "ext"))) %>%
  arrange(set, h, model) %>%
  mutate(model = as.character(model), set = as.character(set))

dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(ev, OUT, row.names = FALSE)

cat(sprintf("Combined %d eval files, %d rows, into %s\n",
            length(files), nrow(ev), OUT))
print(as_tibble(ev), width = Inf, n = Inf)
