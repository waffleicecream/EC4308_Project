# Random forest and bagging for the POOS engine.
#
# Adapted from runrf() in the Lecture 4 code (func-rf.R, Medeiros et al. 2019).
# 01_data_prep.R already adds the lags and aligns x_t with y over t+1..t+h,
# and poos() does the rolling window, so neither embed() nor
# rf.rolling.window() is needed. mtry and nodesize are tuned on the
# validation block. Bagging is the case mtry = p.

suppressPackageStartupMessages(library(randomForest))

NTREE     <- 500
NODESIZES <- c(5, 20, 100, 250, 500)    # 5 = randomForest default for regression
N_WORKERS <- 3                          # parallel jobs (one per horizon)
PROGRESS  <- file.path(CKPT_DIR, "forest_progress.log")

rf_mtry  <- function(p) round(p / c(6, 3, 2))   # p = 120: 20, 40, 60
bag_mtry <- function(p) p

# ---- Predictor groups, for importance only ----------------------------------
# A pair is grouped if its Spearman correlation is at least 0.7 over the first
# training window, or if one is an exact combination of others (de = dp - ep,
# tms = lty - tbl). Each group also takes its members' lags; a predictor in no
# group forms a group with its own lags.
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

# Labels for the importance tables (wording from the workbook's ReadMe sheet)
VAR_DESC <- c(
  dp      = "dividend price ratio",
  dy      = "dividend yield",
  ep      = "earnings price ratio",
  de      = "dividend payout",
  svar    = "stock variance (sigma^2)",
  bm      = "book-to-market (b/m)",
  ntis    = "net equity issuance",
  tbl     = "t-bill",
  lty     = "long govt yield",
  ltr     = "long govt return",
  tms     = "term spread",
  dfy     = "default yield spread",
  dfr     = "default return spread",
  infl    = "inflation",
  lzrt    = "9 illiq measures",
  wtexas  = "oil price changes",
  skvw    = "avg stock skewness",
  tail    = "x-sect tail risk",
  rdsp    = "stock return dispersion",
  dtoy    = "to Dow 52-week high",
  dtoat   = "to Dow all-time high",
  avgcor  = "avg corr stock returns",
  ma_1_9  = "MA signal: price >= 9-month MA (1 = bullish)",
  ma_1_12 = "MA signal: price >= 12-month MA (1 = bullish)",
  ma_2_9  = "MA signal: 2-month MA >= 9-month MA (1 = bullish)",
  ma_2_12 = "MA signal: 2-month MA >= 12-month MA (1 = bullish)",
  ma_3_9  = "MA signal: 3-month MA >= 9-month MA (1 = bullish)",
  ma_3_12 = "MA signal: 3-month MA >= 12-month MA (1 = bullish)",
  mom_9   = "momentum signal: price >= price 9 months ago (1 = bullish)",
  mom_12  = "momentum signal: price >= price 12 months ago (1 = bullish)"
)

# dp_l12 -> dp; infl_l1, infl_l2, infl_l4, infl_l13 -> infl
base_name  <- function(v) sub("_l[0-9]+$", "", v)
lag_months <- function(v) {
  k <- suppressWarnings(as.integer(sub("^.*_l([0-9]+)$", "\\1", v)))
  ifelse(grepl("_l[0-9]+$", v), k, 0L)
}

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

# Every predictor appears in exactly one group, named after its first column
make_groups <- function(vars) {
  g <- lapply(IMP_GROUPS, \(m) vars[base_name(vars) %in% m])
  g <- g[vapply(g, \(x) length(unique(base_name(x))) > 1, logical(1))]
  rest <- setdiff(vars, unlist(g))
  fam  <- split(rest, factor(base_name(rest), unique(base_name(rest))))
  c(g, setNames(fam, vapply(fam, `[`, "", 1)))
}

# ---- Out-of-bag permutation importance --------------------------------------
# As in func-rf.R (importance = TRUE), at the tuning dates, on the forest
# refit on the whole window. For each tree b on its out-of-bag rows:
#   d_b = OOB MSE with the column(s) shuffled - OOB MSE
#   inc_mse_tstat = mean(d_b) / SE     randomForest's "%IncMSE"
#   inc_mse_pct   = 100 * mean(d_b) / mean(OOB MSE)
# Groups (Gregorutti et al. 2015) shuffle all their columns with one row
# order, so near-duplicates (dp, dy) cannot cover for each other. The shuffle
# is drawn once per window rather than per tree.

tree_oob_mse <- function(m, Z, y) {
  oob <- m$inbag == 0
  err <- (y - stats::predict(m, Z, predict.all = TRUE)$individual)^2
  colSums(err * oob) / colSums(oob)
}

oob_shuffle <- function(m, X, y, cols, base) {
  Z <- X
  Z[, cols] <- X[sample(nrow(X)), cols, drop = FALSE]
  tree_oob_mse(m, Z, y) - base
}

summarise_d <- function(d, base) {
  se <- sqrt(mean(d^2) - mean(d)^2) / sqrt(length(d))
  c(inc_mse_tstat = mean(d) / se, inc_mse_pct = 100 * mean(d) / mean(base))
}

oob_importance <- function(X, y, p) {
  m <- randomForest(X, y, ntree = NTREE, mtry = p$mtry, nodesize = p$nodesize,
                    importance = TRUE, keep.inbag = TRUE)
  imp  <- importance(m)
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

# ---- Validation-block permutation importance --------------------------------
# Used by 15_forest_val_importance.R. Out-of-bag months sit next to months the
# tree trained on, so with persistent predictors and overlapping targets the
# OOB measure is inflated. Here one forest is fit on the fit block and the
# shuffles are scored on the validation block (later months only), averaged
# over VAL_REPS shuffles.
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

# ---- Model ------------------------------------------------------------------
forest_model <- function(name) {
  mtry_fn <- switch(name, rf = rf_mtry, bag = bag_mtry)

  fit_forest <- function(X, y, p)
    randomForest(X, y, ntree = NTREE, mtry = p$mtry, nodesize = p$nodesize)
  predict_forest <- function(fit, X) as.numeric(stats::predict(fit, X))

  list(
    name = name,

    # tune_h is kept because inspect() is not given h
    tune = function(X, y, h) {
      grid <- expand.grid(mtry = mtry_fn(ncol(X)), nodesize = NODESIZES)
      best <- tune_grid(X, y, h, grid, fit_forest, predict_forest)
      cat(format(Sys.time()), name, "p =", ncol(X), "h =", h, "tuned: mtry",
          best$mtry, "nodesize", best$nodesize, "\n", file = PROGRESS, append = TRUE)
      cbind(best, tune_h = h)
    },

    fit = function(X, y, p) {
      m <- fit_forest(X, y, p)
      m$spec <- p
      m
    },

    predict = predict_forest,

    inspect = function(fit, X) oob_importance(X, fit$y, fit$spec)
  )
}

# ---- Parallel run with checkpoints ------------------------------------------
# One job per predictor set x horizon. Each finished job is saved to
# output/checkpoints/ and reused on the next run if its spec still matches,
# so a stopped run resumes and a finished one reloads in seconds.
forest_spec <- function(name)
  list(name = name, ntree = NTREE, nodesizes = NODESIZES, seed = SEED,
       roll_window = ROLL_WINDOW, retune = RETUNE_EVERY,
       val_months = VAL_MONTHS, test_end = TEST_END, test_years = TEST_YEARS,
       groups = IMP_GROUPS, importance = "oob_groups_lags")

ckpt_file <- function(name, set, h)
  file.path(CKPT_DIR, sprintf("%s_%s_h%d.rds", name, set, h))

run_forest_job <- function(job) {
  prep <- readRDS(PROC_FILE)
  set.seed(SEED)
  t0 <- Sys.time()
  r  <- poos(forest_model(job$name), prep$data, prep$sets[[job$set]]$vars,
             job$h, set = job$set)
  r$spec <- forest_spec(job$name)
  f <- ckpt_file(job$name, job$set, job$h)
  saveRDS(r, paste0(f, ".tmp"))
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

run_forest <- function(name, sets, horizons = HORIZONS) {
  suppressPackageStartupMessages(library(parallel))
  dir.create(CKPT_DIR, showWarnings = FALSE, recursive = TRUE)

  jobs <- list()
  for (s in names(sets)) for (h in horizons)
    jobs[[length(jobs) + 1]] <- list(name = name, set = s, h = h)
  todo <- Filter(Negate(job_done), jobs)
  cat(sprintf("%s: %d of %d jobs to run (progress: %s)\n",
              name, length(todo), length(jobs), PROGRESS))

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

  res <- combine_runs(lapply(jobs, function(j) readRDS(ckpt_file(j$name, j$set, j$h))))
  res$inspect$importance <- res$inspect$importance |>
    mutate(description = describe_feature(feature), .after = feature)
  res$inspect$group_importance <- res$inspect$group_importance |>
    mutate(description = describe_group(group, set, sets), .after = group)
  res
}
