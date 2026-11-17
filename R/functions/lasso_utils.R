# =============================================================================
# lasso_utils.R  -  helpers shared by the LASSO-type models
#                   (04_lasso.R, 05_post_lasso.R, 06_pca_lasso.R; the models
#                   themselves are in lasso_models.R). new_coef_log() and
#                   log_coef() are also used by 07_ridge.R and 08_kitchen_sink.R.
#
# poos() does not keep the fitted models, so each model's fit() stores its
# coefficient vector in a logger. poos() calls fit() exactly once per origin
# (these models have no tune step), in the same order as the rows of
# res$fits, which is used to label each coefficient vector with its
# model / set / h / origin.
# =============================================================================

# Logger: an environment, so fit() can append to it from inside poos()
new_coef_log <- function() {
  e <- new.env()
  e$b <- list()
  e
}
log_coef <- function(log, b) log$b[[length(log$b) + 1]] <- b

# ---- rlasso (hdm) helpers ---------------------------------------------------
# Do NOT use predict() on rlasso objects here: predict.rlasso() subsets
# newdata[, vars] without drop = FALSE, so a one-row matrix (the forecast
# origin x_t) collapses to a vector and the "forecast" becomes garbage (an
# outer product for post = FALSE, an empty vector for post = TRUE).
# func-rlasso.R avoids this by passing a data frame; here we simply compute
# intercept + x'beta from the stored estimates.
# Also: with post = TRUE and NO predictor selected, $coefficients reports an
# intercept of 0, while $intercept correctly holds mean(y) - so use
# $intercept and $beta, not $coefficients. In that case $beta also loses its
# names, so we match columns by POSITION (X always has the same columns, in
# the same order, at fit and at forecast time) and attach the names ourselves.
rlasso_coef <- function(m, vars) {
  stopifnot(length(m$beta) == length(vars))
  c("(Intercept)" = m$intercept, setNames(as.numeric(m$beta), vars))
}
rlasso_predict <- function(m, X) {
  X <- as.matrix(X)
  stopifnot(ncol(X) == length(m$beta))
  as.numeric(m$intercept + X %*% as.numeric(m$beta))
}

# Turn the log into tables, write them to output/tables, and return them:
#   <name>_coef.csv       coefficients at every origin (long format)
#   <name>_selection.csv  % of origins in which each regressor is nonzero
# plus nsel: number of regressors selected per window (0 -> the forecast is the
# window mean of y, i.e. identical to the prevailing-mean benchmark)
coef_tables <- function(log, res, name) {
  stopifnot(length(log$b) == nrow(res$fits))
  coefs <- bind_rows(lapply(seq_along(log$b), function(k) {
    b <- log$b[[k]]
    data.frame(res$fits[k, c("model", "set", "h", "origin")],
               var = names(b), coef = unname(b), row.names = NULL)
  })) |> filter(var != "(Intercept)")

  selection <- coefs |>
    group_by(var, set, h) |>
    summarise(pct_selected = round(100 * mean(coef != 0)), .groups = "drop") |>
    pivot_wider(names_from = c(set, h), values_from = pct_selected,
                names_sep = "_h", names_sort = TRUE)
  # keep the original regressor order (predictors first, then PCs)
  selection <- selection[match(unique(coefs$var), selection$var), ]

  nsel <- coefs |>
    group_by(set, h, origin) |>
    summarise(n_selected = sum(coef != 0), .groups = "drop") |>
    group_by(set, h) |>
    summarise(min = min(n_selected), median = median(n_selected),
              max = max(n_selected), pct_none = round(100 * mean(n_selected == 0)),
              .groups = "drop")

  dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
  write.csv(coefs, file.path(TAB_DIR, paste0(name, "_coef.csv")), row.names = FALSE)
  write.csv(selection, file.path(TAB_DIR, paste0(name, "_selection.csv")), row.names = FALSE)
  list(coefs = coefs, selection = selection, nsel = nsel)
}

# ---- Parallel runner ----------------------------------------------------------
# The 6 (predictor set x horizon) runs of a model are independent, so they run
# as separate jobs on LASSO_WORKERS R processes (as run_forest() in forest.R),
# about 5x faster than one after the other. Results are identical to a
# sequential run: the models are deterministic and each job is exactly one
# poos() call. Set workers = 1 to run sequentially in the current session.
LASSO_WORKERS <- 6

# One job = one model x one predictor set x one horizon. Runs in a worker:
# rebuilds the model from its type (lasso_models.R), runs poos(), and returns
# the result together with the coefficients logged by fit().
run_lasso_job <- function(job) {
  prep  <- readRDS(PROC_FILE)
  log   <- new_coef_log()
  model <- make_lasso_model(job$type, job$name, log, job$args)
  t0    <- Sys.time()
  r     <- poos(model, prep$data, prep$sets[[job$set]]$vars, job$h, set = job$set)
  stopifnot(length(log$b) == nrow(r$fits))   # one coefficient vector per origin
  r$coefs <- log$b
  r$mins  <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  r
}

# Run one LASSO-type model over all predictor sets and horizons, then save and
# print everything: forecasts, coefficient tables, evaluation vs the benchmark.
#   type : model type in lasso_models.R ("lasso_ic", "rlasso", "post_lasso",
#          "pca_lasso", "pcr_rlasso")
#   name : output name (forecasts/<name>.rds, tables/<name>_*.csv)
#   args : extra settings for the model, e.g. list(n_pc = 8)
run_lasso_type <- function(type, sets, bench, name = type, args = list(),
                           horizons = HORIZONS, workers = LASSO_WORKERS) {
  jobs <- list()
  for (s in names(sets)) for (h in horizons)
    jobs[[length(jobs) + 1]] <- list(type = type, name = name, args = args,
                                     set = s, h = h)
  t0 <- Sys.time()

  if (workers > 1) {
    suppressPackageStartupMessages(library(parallel))
    cl <- makeCluster(min(workers, length(jobs)))
    on.exit(stopCluster(cl), add = TRUE)
    clusterCall(cl, function(wd, lib) { setwd(wd); .libPaths(lib); NULL },
                getwd(), .libPaths())
    clusterEvalQ(cl, {
      source("R/00_config.R")
      source("R/functions/forecast_utils.R")
      source("R/functions/poos.R")
      source("R/functions/lasso_utils.R")
      source("R/functions/lasso_models.R")
      NULL
    })
    runs <- parLapplyLB(cl, jobs, run_lasso_job)
  } else {
    runs <- lapply(jobs, run_lasso_job)
  }

  for (r in runs)
    cat(sprintf("  %s: set=%s h=%d  %d forecasts, train %s..%s (n=%d) at first origin, %.1f min\n",
                name, r$fits$set[1], r$fits$h[1], nrow(r$fc),
                format(r$fits$train_start[1]), format(r$fits$train_end[1]),
                r$fits$n_train[1], r$mins))
  cat("  elapsed:", format(round(Sys.time() - t0, 1)), "\n")

  # Combine in job order; the coefficient list follows the same order as
  # res$fits, which coef_tables() relies on.
  res <- list(fc      = bind_rows(lapply(runs, `[[`, "fc")),
              tuning  = bind_rows(lapply(runs, `[[`, "tuning")),
              inspect = list(),
              fits    = bind_rows(lapply(runs, `[[`, "fits")))
  log <- list(b = do.call(c, lapply(runs, `[[`, "coefs")))

  save_poos(res, name)
  tabs <- coef_tables(log, res, name)
  ev <- evaluate_all(res$fc, bench)
  write.csv(ev, file.path(TAB_DIR, paste0(name, "_eval.csv")), row.names = FALSE)

  cat("\n====", name, "====\n")
  print(ev, width = Inf)
  print(tabs$nsel, width = Inf)
  print(as.data.frame(tabs$selection))
  invisible(list(res = res, eval = ev, tabs = tabs))
}
