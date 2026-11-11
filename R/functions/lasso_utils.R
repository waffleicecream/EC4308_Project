# =============================================================================
# lasso_utils.R  -  helpers shared by the LASSO-type models
#                   (04_lasso.R, 05_post_lasso.R, 06_pca_lasso.R)
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

# Run one LASSO-type model through the POOS engine and save/print everything:
# forecasts, coefficient tables, and the evaluation vs the benchmark.
run_lasso_type <- function(model, log, d, sets, bench, horizons = HORIZONS) {
  res <- poos_all(model, d, sets, horizons)
  save_poos(res, model$name)
  tabs <- coef_tables(log, res, model$name)
  ev <- evaluate_all(res$fc, bench)
  write.csv(ev, file.path(TAB_DIR, paste0(model$name, "_eval.csv")), row.names = FALSE)

  cat("\n====", model$name, "====\n")
  print(ev, width = Inf)
  print(tabs$nsel, width = Inf)
  print(as.data.frame(tabs$selection))
  invisible(list(res = res, eval = ev, tabs = tabs))
}
