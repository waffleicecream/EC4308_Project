# =============================================================================
# 11_ensemble.R  -  equal-weighted mean of the saved model forecasts
#
# Medeiros, Vasconcelos, Veiga, and Zilberman (2019): at each origin the
# combination forecast is the simple average of the individual forecasts.
# Every member has weight 1/N. Nothing is re-estimated, and no weight is fit
# on realised test outcomes (that would need a pre-test history of forecast
# errors, which poos() does not store).
#
# For each predictor set (gw, ext) and horizon h (1, 3, 12), at each origin t:
#   yhat = mean of the member forecasts of y_h made at t
#
# Members: rf, bag, xgb, lasso_bic, rlasso, post_lasso, pca_lasso,
#          pcr_rlasso, ridge.
# Left out: hm (the benchmark this average is judged against), and rf_lag /
#           bag_lag (the same forests with inflation lagged one month).
#
# The script stops if a member file is missing, or if the members do not share
# the same origins and the same realised y.
#
# Outputs: output/forecasts/ensemble_mean.rds
#          output/tables/ensemble_mean_eval.csv   test-period evaluation vs hm
# Run the member scripts and 02_benchmark.R first. Then re-run 04_trading.R.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")

MEMBERS <- c("rf", "bag", "xgb", "lasso_bic", "rlasso", "post_lasso",
             "pca_lasso", "pcr_rlasso", "ridge")

paths   <- file.path(FCST_DIR, paste0(MEMBERS, ".rds"))
missing <- MEMBERS[!file.exists(paths)]
if (length(missing))
  stop("Missing forecast files: ", paste(missing, collapse = ", "),
       ". Run the member scripts first.", call. = FALSE)

members <- bind_rows(lapply(paths, readRDS))
stopifnot(setequal(unique(members$model), MEMBERS))

# One row per member at each origin. y must be the same realised premium.
aligned <- members %>%
  select(model, set, h, date, y, yhat) %>%
  arrange(set, h, date, model)

coverage <- aligned %>%
  group_by(set, h, date) %>%
  summarise(n = n(), n_model = n_distinct(model), n_y = n_distinct(y),
            .groups = "drop")
if (any(coverage$n != length(MEMBERS) | coverage$n_model != length(MEMBERS)))
  stop("Member forecasts do not share the same origins. Each set, horizon, ",
       "and date must contain every member exactly once.", call. = FALSE)
if (any(coverage$n_y != 1))
  stop("Member forecasts disagree on the realised premium y.", call. = FALSE)

wide <- aligned %>%
  select(set, h, date, y, model, yhat) %>%
  pivot_wider(names_from = model, values_from = yhat)
mat <- as.matrix(wide[, MEMBERS])
if (anyNA(mat))
  stop("A member forecast is missing after alignment.", call. = FALSE)

fc <- tibble(model = "ensemble_mean", set = wide$set, h = wide$h,
             date = wide$date, y = wide$y, yhat = rowMeans(mat)) %>%
  arrange(set, h, date)

stopifnot(setequal(unique(fc$set), c("gw", "ext")),
          setequal(unique(fc$h), HORIZONS))

# Spot-check against a direct mean of the long table.
direct <- aligned %>%
  group_by(set, h, date) %>%
  summarise(yhat = mean(yhat), .groups = "drop") %>%
  arrange(set, h, date)
stopifnot(isTRUE(all.equal(fc$yhat, direct$yhat)))

save_forecasts(fc, "ensemble_mean")

bench_path <- file.path(FCST_DIR, "hm.rds")
if (!file.exists(bench_path))
  stop("Missing hm.rds. Run 02_benchmark.R first.", call. = FALSE)
bench <- readRDS(bench_path)
ev <- evaluate_all(fc, bench)
# Every ensemble origin must be scored. A short join means hm lacks that date.
origin_n <- coverage %>% count(set, h, name = "n_origin")
scored   <- inner_join(ev %>% select(set, h, n), origin_n, by = c("set", "h"))
stopifnot(nrow(scored) == nrow(ev), all(scored$n == scored$n_origin))

dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(ev, file.path(TAB_DIR, "ensemble_mean_eval.csv"), row.names = FALSE)

cat(sprintf("ensemble_mean: equal-weighted mean of %d forecasts (weight 1/%d)\n",
            length(MEMBERS), length(MEMBERS)))
cat("Members:", paste(MEMBERS, collapse = ", "), "\n")
print(ev, width = Inf)
