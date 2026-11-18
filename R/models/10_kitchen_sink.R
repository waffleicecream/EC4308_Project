# Kitchen sink (Goyal and Welch 2008, model "all"): OLS of y_h on every
# predictor with an intercept, refit at every origin. Nothing is tuned.
#
# de = dp - ep and tms = lty - tbl (at every lag), so de, tms and their lags
# are dropped, leaving 112 regressors. The fitted values do not depend on
# which column of each identity is dropped; this is checked once on the first
# window, and the identities are checked in every window.
#
# Outputs: output/forecasts/kitchen_sink.rds
#          output/tables/kitchen_sink/kitchen_sink_coef.csv,
#            kitchen_sink_coef_summary.csv, kitchen_sink_eval.csv

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

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

# Dropping de and tms leaves the fitted values unchanged
vars <- prep$sets$ext$vars
h1   <- HORIZONS[1]
tr0  <- d[train_rows(test_origins(d, h1)[1], h1, roll_window(d, h1)),
          c("date", target_col(h1), vars), drop = FALSE] |> drop_na()
X0   <- as.matrix(tr0[vars]); y0 <- tr0[[target_col(h1)]]
check_identities(X0)
full_fit <- lm.fit(cbind(1, X0), y0)$fitted.values
red_fit  <- lm.fit(cbind(1, X0[, keep_cols(X0), drop = FALSE]), y0)$fitted.values
stopifnot(isTRUE(all.equal(full_fit, red_fit, tolerance = ID_TOL)))

ks_log <- new_coef_log()

ks_model <- list(
  name = "kitchen_sink",

  fit = function(X, y, params) {
    check_identities(X)
    keep <- keep_cols(X)
    Xk   <- X[, keep, drop = FALSE]
    Z    <- cbind("(Intercept)" = 1, Xk)
    m    <- lm.fit(Z, y)
    if (m$rank < ncol(Z))
      stop("Kitchen-sink design is rank-deficient (rank ", m$rank, " of ",
           ncol(Z), ").", call. = FALSE)
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

res <- poos_all(ks_model, d, prep$sets, HORIZONS)
save_poos(res, "kitchen_sink")

stopifnot(length(ks_log$b) == nrow(res$fits))
coefs <- bind_rows(lapply(seq_along(ks_log$b), function(k)
  data.frame(res$fits[k, c("model", "set", "h", "origin")], ks_log$b[[k]],
             row.names = NULL))) |>
  filter(var != "(Intercept)")
write.csv(coefs, file.path(model_tab_dir("kitchen_sink"), "kitchen_sink_coef.csv"),
          row.names = FALSE)

coef_summary <- coefs |>
  group_by(var, set, h) |>
  summarise(mean_std_coef = signif(mean(std_coef), 3), .groups = "drop") |>
  pivot_wider(names_from = c(set, h), values_from = mean_std_coef,
              names_sep = "_h", names_sort = TRUE)
coef_summary <- coef_summary[match(unique(coefs$var), coef_summary$var), ]
write.csv(coef_summary, file.path(model_tab_dir("kitchen_sink"), "kitchen_sink_coef_summary.csv"),
          row.names = FALSE)

print(save_eval(res, "kitchen_sink", bench), width = Inf)
