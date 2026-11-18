# Helpers for the LASSO-type models. new_coef_log() and log_coef() are also
# used by ridge and the kitchen sink.
#
# poos() does not keep fitted models, so fit() appends its coefficients to a
# log. fit() runs once per origin in the order of res$fits, which is how each
# coefficient vector is matched to its origin.

new_coef_log <- function() {
  e <- new.env()
  e$b <- list()
  e
}
log_coef <- function(log, b) log$b[[length(log$b) + 1]] <- b

# Do not call predict() on rlasso objects: predict.rlasso() drops a one-row
# matrix to a vector and returns garbage. Also, with post = TRUE and nothing
# selected, $coefficients reports a zero intercept while $intercept holds
# mean(y), and $beta loses its names. So use $intercept and $beta by position.
rlasso_coef <- function(m, vars) {
  stopifnot(length(m$beta) == length(vars))
  c("(Intercept)" = m$intercept, setNames(as.numeric(m$beta), vars))
}
rlasso_predict <- function(m, X) {
  X <- as.matrix(X)
  stopifnot(ncol(X) == length(m$beta))
  as.numeric(m$intercept + X %*% as.numeric(m$beta))
}

# Write <name>_coef.csv (coefficients at every origin) and <name>_selection.csv
# (% of origins each regressor is nonzero). nsel counts regressors per window;
# 0 means the forecast equals the window mean, i.e. the benchmark.
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
  selection <- selection[match(unique(coefs$var), selection$var), ]

  nsel <- coefs |>
    group_by(set, h, origin) |>
    summarise(n_selected = sum(coef != 0), .groups = "drop") |>
    group_by(set, h) |>
    summarise(min = min(n_selected), median = median(n_selected),
              max = max(n_selected), pct_none = round(100 * mean(n_selected == 0)),
              .groups = "drop")

  dir <- model_tab_dir(name)
  write.csv(coefs, file.path(dir, paste0(name, "_coef.csv")), row.names = FALSE)
  write.csv(selection, file.path(dir, paste0(name, "_selection.csv")), row.names = FALSE)
  list(coefs = coefs, selection = selection, nsel = nsel)
}

# ---- Parallel runner --------------------------------------------------------
# Each (set x horizon) run is an independent job on its own R process.
# Results match a sequential run: the models are deterministic.
LASSO_WORKERS <- 3

run_lasso_job <- function(job) {
  prep  <- readRDS(PROC_FILE)
  log   <- new_coef_log()
  model <- make_lasso_model(job$type, job$name, log, job$args)
  t0    <- Sys.time()
  r     <- poos(model, prep$data, prep$sets[[job$set]]$vars, job$h, set = job$set)
  stopifnot(length(log$b) == nrow(r$fits))
  r$coefs <- log$b
  r$mins  <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  r
}

# Run one model type over every set and horizon, then save forecasts,
# coefficient tables and the evaluation.
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
    cat(sprintf("  %s: set=%s h=%d  %d forecasts, %.1f min\n",
                name, r$fits$set[1], r$fits$h[1], nrow(r$fc), r$mins))
  cat("  elapsed:", format(round(Sys.time() - t0, 1)), "\n")

  res <- combine_runs(runs)
  log <- list(b = do.call(c, lapply(runs, `[[`, "coefs")))

  save_poos(res, name)
  tabs <- coef_tables(log, res, name)
  ev   <- save_eval(res, name, bench)

  cat("\n====", name, "====\n")
  print(ev, width = Inf)
  print(tabs$nsel, width = Inf)
  invisible(list(res = res, eval = ev, tabs = tabs))
}
