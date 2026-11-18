# Ridge regression (glmnet, alpha = 0, standardised predictors).
#
# Lambda is tuned on the validation block every 12 origins, not by BIC: ridge
# never sets a coefficient to zero, so BIC's df (= number of nonzero
# coefficients) is always p and BIC would pick the smallest lambda.
# LAMBDA_GRID runs from almost OLS (1e-6) to full shrinkage (1e2, where the
# forecast is the window mean, i.e. the benchmark). The tuning log flags both
# ends: full_shrink = the predictors did not help on validation; at_ols_end =
# the grid should be extended downwards.
#
# Outputs: output/forecasts/ridge.rds
#          output/tables/ridge/ridge_tuning.csv, ridge_coef.csv,
#                              ridge_coef_summary.csv, ridge_eval.csv
# std_coef = coefficient x sd(x) in the window; the summary averages it.

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/metrics.R")
source("R/functions/poos.R")
source("R/functions/lasso_utils.R")
suppressPackageStartupMessages(library(glmnet))

prep  <- readRDS(PROC_FILE)
d     <- prep$data
bench <- readRDS(file.path(FCST_DIR, "hm.rds"))

LAMBDA_GRID <- 10^seq(2, -6, by = -0.1)     # decreasing, as glmnet expects

ridge_path <- function(X, y)
  glmnet(X, y, alpha = 0, lambda = LAMBDA_GRID, standardize = TRUE)

ridge_log <- new_coef_log()

ridge_model <- list(
  name = "ridge",

  # One path on the fit block scores every lambda at once. Must not call
  # ridge_model$fit, or the coefficient log would gain extra entries.
  tune = function(X, y, h) {
    s    <- val_split(nrow(X), h)
    path <- ridge_path(X[s$fit, , drop = FALSE], y[s$fit])
    pred <- predict(path, X[s$val, , drop = FALSE], s = LAMBDA_GRID)
    mse  <- colMeans((y[s$val] - pred)^2)
    j    <- which.min(mse)
    data.frame(lambda       = LAMBDA_GRID[j],
               log10_lambda = log10(LAMBDA_GRID[j]),
               val_mse      = mse[j],
               full_shrink  = j == 1,
               at_ols_end   = j == length(LAMBDA_GRID))
  },

  # The whole path is fit (more stable in glmnet); the tuned lambda is on it
  fit = function(X, y, params) {
    path <- ridge_path(X, y)
    b    <- as.matrix(coef(path, s = params$lambda))[, 1]
    sdx  <- apply(X, 2, sd)
    log_coef(ridge_log, data.frame(var = names(b), coef = unname(b),
                                   std_coef = unname(b * c(NA, sdx[names(b)[-1]]))))
    list(path = path, lambda = params$lambda)
  },

  predict = function(fit, X) as.numeric(predict(fit$path, X, s = fit$lambda))
)

res <- poos_all(ridge_model, d, prep$sets, HORIZONS)
save_poos(res, "ridge")

stopifnot(length(ridge_log$b) == nrow(res$fits))
coefs <- bind_rows(lapply(seq_along(ridge_log$b), function(k)
  data.frame(res$fits[k, c("model", "set", "h", "origin")], ridge_log$b[[k]],
             row.names = NULL))) |>
  filter(var != "(Intercept)")
write.csv(coefs, file.path(model_tab_dir("ridge"), "ridge_coef.csv"), row.names = FALSE)

coef_summary <- coefs |>
  group_by(var, set, h) |>
  summarise(mean_std_coef = signif(mean(std_coef), 3), .groups = "drop") |>
  pivot_wider(names_from = c(set, h), values_from = mean_std_coef,
              names_sep = "_h", names_sort = TRUE)
coef_summary <- coef_summary[match(unique(coefs$var), coef_summary$var), ]
write.csv(coef_summary, file.path(model_tab_dir("ridge"), "ridge_coef_summary.csv"),
          row.names = FALSE)

ev <- save_eval(res, "ridge", bench)

if (any(res$tuning$at_ols_end))
  warning("smallest lambda chosen in ", sum(res$tuning$at_ols_end),
          " tuning(s): extend LAMBDA_GRID downwards")
print(ev, width = Inf)
