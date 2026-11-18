# RF and bagging permutation importance scored on the validation block
# (val_importance() in R/functions/forest.R), at each annual tuning date saved
# in output/checkpoints/. Rebuilds that date's window and reuses its tuned
# mtry and nodesize. The saved runs are only read.
#
# Outputs: output/tables/<model>/<model>_val_importance.csv
#          output/tables/<model>/<model>_val_group_importance.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/poos.R")
source("R/functions/forest.R")
suppressPackageStartupMessages(library(parallel))

prep   <- readRDS(PROC_FILE)
MODELS <- c("rf", "bag")

val_job <- function(job) {
  prep <- readRDS(PROC_FILE)
  d    <- prep$data
  vars <- prep$sets[[job$set]]$vars
  r    <- readRDS(ckpt_file(job$name, job$set, job$h))
  set.seed(SEED)
  yc  <- target_col(job$h)
  win <- roll_window(d, job$h)
  out <- lapply(seq_len(nrow(r$tuning)), function(k) {
    tu <- r$tuning[k, ]
    i  <- which(d$date == tu$origin)
    tr <- d[train_rows(i, job$h, win), c("date", yc, vars), drop = FALSE] |> drop_na()
    stopifnot(nrow(tr) == tu$n_train, min(tr$date) == tu$train_start)
    ins  <- val_importance(as.matrix(tr[vars]), tr[[yc]], tu, job$h)
    info <- data.frame(model = job$name, set = job$set, h = job$h, origin = tu$origin)
    lapply(ins, \(x) cbind(info, x, row.names = NULL))
  })
  list(importance       = bind_rows(lapply(out, `[[`, "importance")),
       group_importance = bind_rows(lapply(out, `[[`, "group_importance")))
}

jobs <- list()
for (m in MODELS) for (s in names(prep$sets)) for (h in HORIZONS)
  jobs[[length(jobs) + 1]] <- list(name = m, set = s, h = h)
if (!all(vapply(jobs, \(j) file.exists(ckpt_file(j$name, j$set, j$h)), logical(1))))
  stop("Missing checkpoints in ", CKPT_DIR, ". Run 03_rf.R and 04_bag.R first.", call. = FALSE)

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
    mutate(description = describe_group(group, set, prep$sets), .after = group)
  write.csv(imp, file.path(model_tab_dir(m), paste0(m, "_val_importance.csv")), row.names = FALSE)
  write.csv(grp, file.path(model_tab_dir(m), paste0(m, "_val_group_importance.csv")), row.names = FALSE)
}
cat("elapsed:", format(round(Sys.time() - t0, 1)), "\n")
