# =============================================================================
# 08_kitchen_sink.R  -  OLS on every predictor, evaluated with the shared
#                       POOS engine (poos.R)
#
# The kitchen sink (Goyal and Welch 2008, their model "all"): at each origin,
# OLS of the h-month log equity premium on the whole predictor set, with an
# intercept. Nothing is tuned and nothing is shrunk.
#
# de = dp - ep and tms = lty - tbl (01_data_prep.R), so those two columns, and
# their 1, 3 and 12-month lags, lie in the span of the others. They are
# dropped (gw: 48 regressors, ext: 112) and the series they are built from
# are kept. Within each trio the fitted values do not depend on which
# column is removed; this is checked once, on the first gw h = 1 window,
# against lm.fit of the full set. The identity itself is checked in every
# window. If the reduced design is still rank-deficient, the script stops.
#
# Outputs: output/forecasts/kitchen_sink.rds
#          output/tables/kitchen_sink_coef.csv
#          output/tables/kitchen_sink_coef_summary.csv
#          output/tables/kitchen_sink_eval.csv   test-period evaluation vs hm
# Picked up by 09_ensemble.R and 10_trading.R.
#
# Run 01_data_prep.R and 02_benchmark.R first.
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")   # new_coef_log(), log_coef()

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))     # run 02_benchmark.R first

# Constructed variables that are exact linear combinations of other predictors.
# The identities hold at every lag (01_data_prep.R adds _l1, _l3, _l12), so
# the lags of de and tms are dropped too.
SUFFIX <- c("", paste0("_l", prep$notes$lags))
DROP   <- c(paste0("de", SUFFIX), paste0("tms", SUFFIX))
ID_TOL <- 1e-8

check_identities <- function(X) {
  stopifnot(all(DROP %in% colnames(X)))
  for (s in SUFFIX) {
    v <- \(x) X[, paste0(x, s)]
    stopifnot(max(abs(v("de")  - (v("dp")  - v("ep"))))  < ID_TOL)
    stopifnot(max(abs(v("tms") - (v("lty") - v("tbl")))) < ID_TOL)
  }
}

keep_cols <- function(X) setdiff(colnames(X), DROP)

# Fitted values of the full set match the reduced set: de and tms add no
# column space. One window is enough; the identities are algebraic.
h1  <- HORIZONS[1]
i0  <- test_origins(d, h1)[1]
yc0 <- target_col(h1)
tr0 <- d[train_rows(i0, h1, roll_window(d, h1)),
         c("date", yc0, prep$sets$gw$vars), drop = FALSE] |> drop_na()
X0  <- as.matrix(tr0[prep$sets$gw$vars]); y0 <- tr0[[yc0]]
check_identities(X0)
full_fit <- lm.fit(cbind(1, X0), y0)$fitted.values
red_fit  <- lm.fit(cbind(1, X0[, keep_cols(X0), drop = FALSE]), y0)$fitted.values
stopifnot(isTRUE(all.equal(full_fit, red_fit, tolerance = ID_TOL)))

# ---- Model definition (POOS interface: fit / predict) -----------------------
ks_log <- new_coef_log()   # coefficients of every window (one per origin)

ks_model <- list(
  name = "kitchen_sink",

  fit = function(X, y, params) {
    check_identities(X)
    keep <- keep_cols(X)
    Xk   <- X[, keep, drop = FALSE]
    Z    <- cbind("(Intercept)" = 1, Xk)
    m    <- lm.fit(Z, y)
    if (m$rank < ncol(Z))
      stop("Reduced kitchen-sink design is rank-deficient (rank ",
           m$rank, " of ", ncol(Z), ").", call. = FALSE)
    b <- m$coefficients
    names(b) <- colnames(Z)
    sdx <- apply(Xk, 2, stats::sd)
    log_coef(ks_log, data.frame(
      var = names(b), coef = unname(b),
      std_coef = unname(c(NA_real_, b[-1] * sdx[names(b)[-1]])),
      row.names = NULL))
    list(coef = b, keep = keep)
  },

  predict = function(fit, X)
    as.numeric(cbind(1, X[, fit$keep, drop = FALSE]) %*% fit$coef)
)

# ---- Run ----------------------------------------------------------------------
cat("Kitchen sink: OLS on every predictor except de, tms and their lags, refit at every origin\n")
res <- poos_all(ks_model, d, prep$sets, HORIZONS)
save_poos(res, "kitchen_sink")

# ---- Coefficient tables -----------------------------------------------------
stopifnot(length(ks_log$b) == nrow(res$fits))
coefs <- bind_rows(lapply(seq_along(ks_log$b), function(k)
  data.frame(res$fits[k, c("model", "set", "h", "origin")], ks_log$b[[k]],
             row.names = NULL))) |>
  filter(var != "(Intercept)")
write.csv(coefs, file.path(TAB_DIR, "kitchen_sink_coef.csv"), row.names = FALSE)

# Average standardized coefficient (effect of a 1-sd move in x on y_h) over
# the test origins, one column per set x horizon; predictors in data order.
coef_summary <- coefs |>
  group_by(var, set, h) |>
  summarise(mean_std_coef = signif(mean(std_coef), 3), .groups = "drop") |>
  pivot_wider(names_from = c(set, h), values_from = mean_std_coef,
              names_sep = "_h", names_sort = TRUE)
coef_summary <- coef_summary[match(unique(coefs$var), coef_summary$var), ]
write.csv(coef_summary, file.path(TAB_DIR, "kitchen_sink_coef_summary.csv"),
          row.names = FALSE)

# ---- Evaluate on the test period vs the prevailing-mean benchmark -----------
ev <- evaluate_all(res$fc, bench)
write.csv(ev, file.path(TAB_DIR, "kitchen_sink_eval.csv"), row.names = FALSE)

cat("\n==== kitchen sink: evaluation vs prevailing mean ====\n")
print(ev, width = Inf)
cat("\n==== kitchen sink: mean standardized coefficients ====\n")
print(as.data.frame(coef_summary))
