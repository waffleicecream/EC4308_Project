# =============================================================================
# forest_paths.R  -  output folders and predictor descriptions for the RF and
#                    bagging scripts
#
# Kept here rather than in 00_config.R / poos.R so the shared files stay as
# they are for the other models. Only the forest scripts use per-model table
# folders: output/tables/<model>/ (e.g. rf/rf_tuning.csv).
#
# Sourced by forest.R and forest_interpret.R.
# =============================================================================

# Per-model table folder, output/tables/<model>/. Creates it if needed.
model_tab_dir <- function(model) {
  d <- file.path(TAB_DIR, model)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# Same as save_poos() in poos.R, but the tuning / inspect logs go in
# output/tables/<name>/ instead of output/tables/. Forecasts still go to
# output/forecasts/<name>.rds, so 04_trading.R picks them up as usual.
save_forest_poos <- function(res, name) {
  save_forecasts(res$fc, name)
  dir <- model_tab_dir(name)
  if (nrow(res$tuning))
    write.csv(res$tuning, file.path(dir, paste0(name, "_tuning.csv")), row.names = FALSE)
  for (nm in names(res$inspect)) if (nrow(res$inspect[[nm]]))
    write.csv(res$inspect[[nm]], file.path(dir, paste0(name, "_", nm, ".csv")),
              row.names = FALSE)
}

# ---- Predictor descriptions (for the importance tables) ---------------------
# Wording from the ReadMe sheet of Data2025.xlsx. The technical indicators are
# not in the file (built in 01_data_prep.R), so their wording is ours.
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
  infl_l1 = "inflation, lagged 1 month (CPI is released the following month)",
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
