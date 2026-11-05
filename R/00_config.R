# =============================================================================
# 00_config.R  -  global settings shared by every script.
# Source this at the top of each script:  source("R/00_config.R")
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# ---- Paths ------------------------------------------------------------------
RAW_FILE   <- "data/Data2025.xlsx"
PROC_FILE  <- "data/processed/monthly.rds"
TAB_DIR    <- "output/tables"
FIG_DIR    <- "output/figures"
FCST_DIR   <- "output/forecasts"

# ---- Forecasting design -----------------------------------------------------
HORIZONS     <- c(1, 3, 12)              # months; also the rebalance frequency
TEST_START   <- as.Date("1997-01-31")    # first month whose return is forecast
TEST_END     <- as.Date("2025-12-31")    # last month whose return is forecast
FIRST_ORIGIN <- as.Date("1996-12-31")    # first forecast origin (= TEST_START - 1m)

# Models: rolling window of the most recent WINDOW months (train), then
# forecast the next h months (test). No separate validation set, except XGBoost.
WINDOW         <- 180                    # rolling training window (months)
RETUNE_EVERY   <- 12                     # re-tune hyperparameters every 12 origins
XGB_VAL_MONTHS <- 36                     # XGBoost only: last 36 months of the
                                         # training window used for early stopping

# Benchmark: prevailing mean (Goyal & Welch 2008) = average of ALL past
# realised equity premia up to the forecast origin (expanding window).

MODELS <- list(
  penalised  = c("ridge", "lasso", "rlasso"),
  dimred     = c("pcr_rlasso", "pca_x_rlasso", "pls"),
  trees      = c("bagging", "rf", "xgb")
)

# ---- Trading ----------------------------------------------------------------
# Strategy: every h months, 100% S&P 500 if forecast > 0, else 100% T-bills
# (Rfree). Long-only, no leverage. Start in T-bills.
W0           <- 1e6                      # starting wealth
COST_PER_LEG <- 0.001                    # 0.1% per trade (each buy or sell)
COST         <- 2 * COST_PER_LEG         # a switch = sell one + buy other = 0.2%
SEED  <- 4308
