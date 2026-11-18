# Equal-weighted forecast combination: at each origin, the simple mean of the
# ten model forecasts (weight 1/10 each). No weight is fit on test outcomes.
# Stops if a member is missing or the members disagree on origins or y.
#
# Outputs: output/forecasts/ensemble_mean.rds
#          output/tables/ensemble_mean/ensemble_mean_eval.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")

MEMBERS <- c("rf", "bag", "xgb", "lasso_bic", "rlasso", "post_lasso",
             "pca_lasso", "pcr_rlasso", "ridge", "kitchen_sink")

paths   <- file.path(FCST_DIR, paste0(MEMBERS, ".rds"))
missing <- MEMBERS[!file.exists(paths)]
if (length(missing))
  stop("Missing forecast files: ", paste(missing, collapse = ", "),
       ". Run the model scripts first.", call. = FALSE)

members <- bind_rows(lapply(paths, readRDS)) |>
  select(model, set, h, date, y, yhat)
stopifnot(setequal(unique(members$model), MEMBERS))

coverage <- members |>
  group_by(set, h, date) |>
  summarise(n = n(), n_model = n_distinct(model), n_y = n_distinct(y),
            .groups = "drop")
if (any(coverage$n != length(MEMBERS) | coverage$n_model != length(MEMBERS)))
  stop("Member forecasts do not share the same origins.", call. = FALSE)
if (any(coverage$n_y != 1))
  stop("Member forecasts disagree on the realised premium y.", call. = FALSE)

wide <- members |>
  arrange(set, h, date) |>
  pivot_wider(names_from = model, values_from = yhat)
mat <- as.matrix(wide[, MEMBERS])
stopifnot(!anyNA(mat))

fc <- tibble(model = "ensemble_mean", set = wide$set, h = wide$h,
             date = wide$date, y = wide$y, yhat = rowMeans(mat))
stopifnot(setequal(unique(fc$h), HORIZONS), nrow(fc) == nrow(coverage))

save_forecasts(fc, "ensemble_mean")

bench <- readRDS(file.path(FCST_DIR, "hm.rds"))
ev <- evaluate_all(fc, bench)
stopifnot(all(ev$n == (coverage |> count(set, h))$n))   # every origin scored
write.csv(ev, file.path(model_tab_dir("ensemble_mean"), "ensemble_mean_eval.csv"),
          row.names = FALSE)

cat("ensemble_mean: mean of", paste(MEMBERS, collapse = ", "), "\n")
print(ev, width = Inf)
