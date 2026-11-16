# =============================================================================
# forest.R  -  random forest and bagging models for the POOS engine (poos.R)
#
# Adapted from runrf() in the Lecture 4 code, func-rf.R (Medeiros et al. 2019).
# Same fit and predict calls; the changes our data requires are:
#   - no dummy column or principal components, and no embed() here: 01_data_prep.R
#     already adds the 1, 3 and 12-month lags of every predictor and aligns
#     row t = x_t with y over t+1..t+h, and poos.R handles timing
#   - mtry and nodesize are tuned on the validation block (poos.R, tune_grid)
#   - the rolling window loop is poos(), so rf.rolling.window() is not needed
#
# RF and bagging differ only in mtry. Bagging is the case mtry = p.
#
# Sourced by 03_rf.R and 03_bag.R, and by each parallel worker.
# =============================================================================

suppressPackageStartupMessages(library(randomForest))
source("R/functions/forest_paths.R")   # model_tab_dir(), save_forest_poos(), VAR_DESC

# ---- Settings ---------------------------------------------------------------
NTREE       <- 500               # fixed: more trees reduce noise, so not tuned
NODESIZES   <- c(5, 20, 100, 250, 500)  # 5 = randomForest default for regression;
                                        # 250, 500 added because 100 (old max) was
                                        # chosen at almost every tune
FOREST_SETS <- c("gw", "ext")    # predictor sets run for RF / bagging
N_WORKERS   <- 6                 # one per predictor set x horizon
CKPT_DIR    <- "output/checkpoints"
PROGRESS    <- "output/forest_progress.log"

# mtry candidates, as shares of the number of predictors p. p/3 is the
# regression default (Lecture 3.2). gw (p = 56): 9, 19, 28. ext (p = 120): 20, 40, 60.
rf_mtry  <- function(p) round(p / c(6, 3, 2))
bag_mtry <- function(p) p        # every predictor at every split

# ---- Predictor groups, for importance only ----------------------------------
# The models still see every predictor separately. A pair is grouped if its
# Spearman correlation is at least 0.7 over the first training window, or if
# one is an exact combination of others (de = dp - ep, tms = lty - tbl).
# Groups are listed by base predictor; each group also takes the members'
# lags (_l1, _l3, _l12). A predictor in no group forms a group with its own
# lags, so a lag cannot cover for the current value when it is shuffled.
IMP_GROUPS <- list(
  valuation = c("dp", "dy", "ep", "de", "bm"),
  rates     = c("tbl", "lty", "tms"),
  trend     = c("ma_1_9", "ma_1_12", "ma_2_9", "ma_2_12", "ma_3_9", "ma_3_12",
                "mom_9", "mom_12", "dtoy"),
  risk      = c("svar", "avgcor")
)
GROUP_DESC <- c(
  valuation = "valuation ratios",
  rates     = "interest rates",
  trend     = "trend / technical signals",
  risk      = "stock market risk"
)

# Base predictor of a column: dp_l12 -> dp. infl_l1 (inflation, lagged for the
# CPI release) and its lags infl_l2, infl_l4, infl_l13 -> infl.
base_name <- function(v) sub("_l[0-9]+$", "", v)
lag_months <- function(v) {
  k <- suppressWarnings(as.integer(sub("^.*_l([0-9]+)$", "\\1", v)))
  ifelse(grepl("_l[0-9]+$", v), k, 0L)
}

# Description column for the importance tables: VAR_DESC (forest_paths.R) for a
# single variable, plus how many months back for a lag; for a group, its label
# plus the base members present in the set.
describe_feature <- function(feature) {
  k <- lag_months(feature)
  unname(ifelse(k == 0, VAR_DESC[base_name(feature)],
                paste0(VAR_DESC[base_name(feature)], ", ", k, " month",
                       ifelse(k == 1, "", "s"), " earlier")))
}
describe_group <- function(group, set, sets) {
  vapply(seq_along(group), function(i) {
    g <- group[i]
    if (!g %in% names(IMP_GROUPS))
      return(paste(describe_feature(g), "(with its lags)"))
    members <- intersect(IMP_GROUPS[[g]], base_name(sets[[set[i]]]$vars))
    paste0(GROUP_DESC[[g]], ": ", paste(members, collapse = ", "), " (with lags)")
  }, character(1))
}

# Groups for a given predictor set: every predictor appears exactly once. A
# group in IMP_GROUPS takes its members and their lags; any other predictor
# forms a group with its own lags, named after the first column of the family
# (dp, ntis, infl_l1, ...).
make_groups <- function(vars) {
  g <- lapply(IMP_GROUPS, \(m) vars[base_name(vars) %in% m])
  g <- g[vapply(g, \(x) length(unique(base_name(x))) > 1, logical(1))]
  rest <- setdiff(vars, unlist(g))
  fam  <- split(rest, factor(base_name(rest), unique(base_name(rest))))
  c(g, setNames(fam, vapply(fam, `[`, "", 1)))
}

# ---- Importance -------------------------------------------------------------
# Out-of-bag permutation importance, as in the Lecture 4 code (func-rf.R:
# randomForest(..., importance = TRUE), then importance(model)), at the tuning
# dates only, on a refit of the whole training window with the chosen settings.
#
# For each tree b, on its out-of-bag (OOB) rows:
#   base_b = OOB MSE,  d_b = OOB MSE with the column(s) shuffled - base_b
# Two summaries over the NTREE trees:
#   inc_mse_tstat = mean(d_b) / SE(mean)      randomForest's "%IncMSE" (not a %)
#   inc_mse_pct   = 100 * mean(d_b) / mean(base_b)   actual % rise in OOB MSE
#
# Single variables: inc_mse_tstat is importance(model) itself.
# Groups (Gregorutti, Michel & Saint-Pierre 2015): randomForest cannot shuffle
# several columns together, so oob_shuffle() repeats its calculation by hand,
# shuffling a group's columns with the SAME row order so near-duplicates
# (dp, dy) cannot cover for each other. One difference: randomForest shuffles
# within each tree's OOB rows; we shuffle once over the window and score each
# tree on its own OOB rows (same distribution, far faster).
# inc_node_purity: total RSS drop from splits on the variable, in-bag.

# Per-tree OOB MSE of forest m (fit with keep.inbag = TRUE) on predictors Z
tree_oob_mse <- function(m, Z, y) {
  oob <- m$inbag == 0
  err <- (y - stats::predict(m, Z, predict.all = TRUE)$individual)^2
  colSums(err * oob) / colSums(oob)
}

# Shuffle columns `cols` of X once, return d_b for every tree
oob_shuffle <- function(m, X, y, cols, base) {
  Z <- X
  Z[, cols] <- X[sample(nrow(X)), cols, drop = FALSE]
  tree_oob_mse(m, Z, y) - base
}

summarise_d <- function(d, base) {
  se <- sqrt(mean(d^2) - mean(d)^2) / sqrt(length(d))   # as randomForest
  c(inc_mse_tstat = mean(d) / se, inc_mse_pct = 100 * mean(d) / mean(base))
}

oob_importance <- function(X, y, p) {
  m <- randomForest(X, y, ntree = NTREE, mtry = p$mtry, nodesize = p$nodesize,
                    importance = TRUE, keep.inbag = TRUE)
  imp  <- importance(m)                       # lecture: %IncMSE, IncNodePurity
  base <- tree_oob_mse(m, X, y)
  vars <- colnames(X)

  single_pct <- vapply(vars, function(v)
    summarise_d(oob_shuffle(m, X, y, v, base), base)[["inc_mse_pct"]], numeric(1))
  groups <- make_groups(vars)
  grp    <- t(vapply(groups, function(cols)
    summarise_d(oob_shuffle(m, X, y, cols, base), base), numeric(2)))

  list(
    importance = data.frame(feature = vars,
                            inc_mse_tstat   = imp[vars, "%IncMSE"],
                            inc_mse_pct     = unname(single_pct),
                            inc_node_purity = imp[vars, "IncNodePurity"],
                            row.names = NULL),
    group_importance = data.frame(group = names(groups), n_vars = lengths(groups),
                                  grp, row.names = NULL)
  )
}

# ---- Validation-block importance (run by R/14_forest_val_importance.R) -------
# The same shuffles as oob_importance(), but scored on later months only:
#   1. fit one forest on the fit block (val_split() in poos.R), with the
#      settings chosen at that tuning date
#   2. base = MSE on the validation block (last VAL_MONTHS rows)
#   3. shuffle a variable's (or a group's) validation rows, predict, new MSE
#   4. inc_mse_pct = 100 * (new MSE - base) / base, averaged over VAL_REPS
#      shuffles (one shuffle is a single random draw)
# The forest has seen nothing from the validation block, and the h - 1 gap
# keeps targets from overlapping, so neighbouring months cannot leak as they
# do out-of-bag. Measures what helps the forest forecast later months, not
# what it uses to fit the training data.
VAL_REPS <- 10

val_importance <- function(X, y, p, h) {
  s    <- val_split(nrow(X), h)
  m    <- randomForest(X[s$fit, , drop = FALSE], y[s$fit], ntree = NTREE,
                       mtry = p$mtry, nodesize = p$nodesize)
  Xval <- X[s$val, , drop = FALSE]
  yval <- y[s$val]
  mse  <- function(Z) mean((yval - stats::predict(m, Z))^2)
  base <- mse(Xval)

  shuffle_score <- function(cols) {
    shuffled <- replicate(VAL_REPS, {
      Z <- Xval
      Z[, cols] <- Xval[sample(nrow(Xval)), cols, drop = FALSE]
      mse(Z)
    })
    c(inc_mse = mean(shuffled) - base,
      inc_mse_pct = 100 * (mean(shuffled) - base) / base)
  }

  vars   <- colnames(X)
  groups <- make_groups(vars)
  list(
    importance = data.frame(feature = vars,
                            t(vapply(vars, shuffle_score, numeric(2))),
                            row.names = NULL),
    group_importance = data.frame(group = names(groups), n_vars = lengths(groups),
                                  t(vapply(groups, shuffle_score, numeric(2))),
                                  row.names = NULL)
  )
}

# ---- Model definitions (POOS interface: tune / fit / predict / inspect) -----
forest_model <- function(name) {
  mtry_fn <- switch(name, rf = rf_mtry, bag = bag_mtry)

  fit_forest <- function(X, y, p)
    randomForest(X, y, ntree = NTREE, mtry = p$mtry, nodesize = p$nodesize)
  predict_forest <- function(fit, X) as.numeric(stats::predict(fit, X))

  list(
    name = name,

    # h is stored with the chosen settings because inspect() needs it for the
    # validation split and the engine does not pass it there.
    tune = function(X, y, h) {
      grid <- expand.grid(mtry = mtry_fn(ncol(X)), nodesize = NODESIZES)
      best <- tune_grid(X, y, h, grid, fit_forest, predict_forest)
      cat(format(Sys.time()), name, "p =", ncol(X), "h =", h, "tuned: mtry",
          best$mtry, "nodesize", best$nodesize, "\n", file = PROGRESS, append = TRUE)
      cbind(best, tune_h = h)
    },

    fit = function(X, y, p) {
      m <- fit_forest(X, y, p)
      m$spec <- p            # kept for inspect(), and y is stored as m$y
      m
    },

    predict = predict_forest,

    inspect = function(fit, X) oob_importance(X, fit$y, fit$spec)
  )
}

# ---- Parallel run, one job per predictor set x horizon ----------------------
# Everything that defines a run. A saved job is reused only if this matches.
forest_spec <- function(name)
  list(name = name, ntree = NTREE, nodesizes = NODESIZES, seed = SEED,
       roll_window = ROLL_WINDOW, retune = RETUNE_EVERY,
       val_months = VAL_MONTHS, test_end = TEST_END, test_years = TEST_YEARS,
       groups = IMP_GROUPS, importance = "oob_groups_lags")

# Keep only the predictor sets the forest scripts run (FOREST_SETS)
forest_sets <- function(sets) sets[FOREST_SETS]

ckpt_file <- function(name, set, h)
  file.path(CKPT_DIR, sprintf("%s_%s_h%d.rds", name, set, h))

# Each job is saved when it finishes, so a stopped run resumes where it left
# off. Checkpoints sit outside FCST_DIR because 10_trading.R reads all of it.
run_forest_job <- function(job) {
  prep <- readRDS(PROC_FILE)
  set.seed(SEED)                        # each job reproducible on its own
  t0 <- Sys.time()
  r  <- poos(forest_model(job$name), prep$data, prep$sets[[job$set]]$vars,
             job$h, set = job$set)
  r$spec <- forest_spec(job$name)
  f <- ckpt_file(job$name, job$set, job$h)
  saveRDS(r, paste0(f, ".tmp"))          # never leave a half-written checkpoint
  file.rename(paste0(f, ".tmp"), f)
  cat(format(Sys.time()), "DONE", job$name, job$set, "h =", job$h, "in",
      round(as.numeric(difftime(Sys.time(), t0, units = "mins"))), "min\n",
      file = PROGRESS, append = TRUE)
  invisible(NULL)
}

job_done <- function(job) {
  f <- ckpt_file(job$name, job$set, job$h)
  file.exists(f) && identical(readRDS(f)$spec, forest_spec(job$name))
}

# Runs the unfinished jobs on N_WORKERS R processes, then combines all of them
# into the structure poos_all() returns, so save_forest_poos() and evaluate_all() apply.
run_forest <- function(name, sets, horizons = HORIZONS) {
  suppressPackageStartupMessages(library(parallel))
  dir.create(CKPT_DIR, showWarnings = FALSE, recursive = TRUE)

  jobs <- list()
  for (s in names(sets)) for (h in horizons)
    jobs[[length(jobs) + 1]] <- list(name = name, set = s, h = h)
  todo <- Filter(Negate(job_done), jobs)
  cat(sprintf("%s: %d of %d jobs to run\n", name, length(todo), length(jobs)))

  if (length(todo) > 0) {
    cl <- makeCluster(min(N_WORKERS, length(todo)))
    on.exit(stopCluster(cl), add = TRUE)
    clusterCall(cl, function(wd, lib) { setwd(wd); .libPaths(lib); NULL },
                getwd(), .libPaths())
    clusterEvalQ(cl, {
      source("R/00_config.R")
      source("R/functions/forecast_utils.R")
      source("R/functions/poos.R")
      source("R/functions/forest.R")
      NULL
    })
    parLapplyLB(cl, todo, run_forest_job)
  }

  runs <- lapply(jobs, function(j) readRDS(ckpt_file(j$name, j$set, j$h)))
  insp_names <- unique(unlist(lapply(runs, \(r) names(r$inspect))))
  inspect <- setNames(lapply(insp_names, \(nm)
               bind_rows(lapply(runs, \(r) r$inspect[[nm]]))), insp_names)

  # Descriptions are added here, not in the workers, so cached checkpoints
  # get them too.
  inspect$importance <- inspect$importance |>
    mutate(description = describe_feature(feature), .after = feature)
  inspect$group_importance <- inspect$group_importance |>
    mutate(description = describe_group(group, set, sets), .after = group)

  list(fc      = bind_rows(lapply(runs, `[[`, "fc")),
       tuning  = bind_rows(lapply(runs, `[[`, "tuning")),
       inspect = inspect,
       fits    = bind_rows(lapply(runs, `[[`, "fits")))
}
