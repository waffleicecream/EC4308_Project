# =============================================================================
# 05_forest_val_importance.R  -  RF and bagging importance on the validation
#                                block (alternative to the out-of-bag measure)
#
# For each model (rf, bag) x predictor set (FOREST_SETS) x horizon, at each of the 10 annual
# tuning dates logged by 03_rf.R / 03_bag.R:
#   rebuild that date's training window, take the mtry / nodesize chosen there,
#   fit one forest on the fit block and score shuffles on the last 240 months
#   (val_importance() in R/functions/forest.R)
# Forecasts and the saved runs (output/checkpoints) are only read, never changed.
#
# Why: out-of-bag months sit next to months each tree trained on, so with
# persistent predictors and overlapping h-month targets the OOB importance
# (03_rf.R) is inflated. Here the forest is scored only on months after
# everything it trained on.
#
# Outputs: output/tables/<model>/<model>_val_importance.csv        one variable at a time
#          output/tables/<model>/<model>_val_group_importance.csv  groups (forest.R)
# Run 03_rf.R and 03_bag.R first; plots: 05_rf_interpret.R, 05_bag_interpret.R.
# Run time: about 5 minutes (parallel).
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/poos.R")
source("R/functions/forest.R")
suppressPackageStartupMessages(library(parallel))

prep <- readRDS(PROC_FILE)

# rf_lag / bag_lag: same models with lagged inflation (lag_infl() in forest.R)
MODELS   <- c("rf", "bag", "rf_lag", "bag_lag")
is_lag   <- function(m) grepl("_lag$", m)
sets_for <- function(m) forest_sets(if (is_lag(m)) lag_infl(prep)$sets else prep$sets)

# One job = one saved run (model x set x h), all 10 tuning dates
val_job <- function(job) {
  prep <- readRDS(PROC_FILE)
  if (grepl("_lag$", job$name)) prep <- lag_infl(prep)
  d <- prep$data
  vars <- prep$sets[[job$set]]$vars
  r    <- readRDS(ckpt_file(job$name, job$set, job$h))
  set.seed(SEED)                         # each job reproducible on its own
  yc  <- target_col(job$h)
  win <- roll_window(d, job$h)
  out <- lapply(seq_len(nrow(r$tuning)), function(k) {
    tu <- r$tuning[k, ]
    i  <- which(d$date == tu$origin)
    tr <- d[train_rows(i, job$h, win), c("date", yc, vars), drop = FALSE] |> drop_na()
    stopifnot(nrow(tr) == tu$n_train, min(tr$date) == tu$train_start)  # same window
    ins  <- val_importance(as.matrix(tr[vars]), tr[[yc]], tu, job$h)
    info <- data.frame(model = job$name, set = job$set, h = job$h, origin = tu$origin)
    lapply(ins, \(x) cbind(info, x, row.names = NULL))
  })
  list(importance       = bind_rows(lapply(out, `[[`, "importance")),
       group_importance = bind_rows(lapply(out, `[[`, "group_importance")))
}

jobs <- list()
for (m in MODELS) for (s in FOREST_SETS) for (h in HORIZONS)
  jobs[[length(jobs) + 1]] <- list(name = m, set = s, h = h)
stopifnot(all(vapply(jobs, \(j) file.exists(ckpt_file(j$name, j$set, j$h)), logical(1))))

t0 <- Sys.time()
cl <- makeCluster(min(N_WORKERS, length(jobs)))
invisible(clusterCall(cl, function(wd, lib) { setwd(wd); .libPaths(lib); NULL },
                      getwd(), .libPaths()))
invisible(clusterEvalQ(cl, {
  source("R/00_config.R")
  source("R/functions/forecast_utils.R")
  source("R/functions/poos.R")
  source("R/functions/forest.R")
  NULL
}))
res <- parLapplyLB(cl, jobs, val_job)
stopCluster(cl)

for (m in MODELS) {
  rm_ <- res[vapply(jobs, \(j) j$name == m, logical(1))]
  imp <- bind_rows(lapply(rm_, `[[`, "importance")) |>
    mutate(description = describe_feature(feature), .after = feature)
  grp <- bind_rows(lapply(rm_, `[[`, "group_importance")) |>
    mutate(description = describe_group(group, set, sets_for(m)), .after = group)
  write.csv(imp, file.path(model_tab_dir(m), paste0(m, "_val_importance.csv")), row.names = FALSE)
  write.csv(grp, file.path(model_tab_dir(m), paste0(m, "_val_group_importance.csv")), row.names = FALSE)
}
cat("elapsed:", format(round(Sys.time() - t0, 1)), "\n")
