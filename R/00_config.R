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
FCST_DIR   <- "output/forecasts"
FIG_DIR    <- "output/figures"

# ---- Forecasting design -----------------------------------------------------
HORIZONS     <- c(1, 3, 12)              # months; also the rebalance frequency
TRAIN_START  <- as.Date("1926-12-31")    # first month used for training (all
                                         # sets): first month every predictor
                                         # (last to start: ntis) is available.
                                         # Earlier rows are dropped.
TEST_YEARS   <- 30                       # length of the test period
TEST_END     <- as.Date("2025-12-31")    # last month whose return is forecast
month_end    <- function(d) floor_date(d, "month") %m+% months(1) - days(1)
TEST_START   <- month_end(TEST_END %m-% months(12 * TEST_YEARS - 1))  # 1996-01-31
FIRST_ORIGIN <- month_end(floor_date(TEST_START, "month") - days(1))  # 1995-12-31

# Pseudo-out-of-sample with a ROLLING window, used by EVERY model and the
# benchmark through R/functions/poos.R. At each test origin the model is
# re-estimated on the most recent ROLL_WINDOW months, then forecasts the next
# h months; each step forward adds the newest month and drops the oldest.
# No data realised after the origin is ever used.
#   ROLL_WINDOW = NULL -> window length = all data before the test period
#                         (starts at TRAIN_START = 1926-12 at the first origin,
#                         828 months for h = 1: a 588-month fit block and a
#                         240-month validation block). The window is fixed in
#                         length and rolls, so at the last origin it starts
#                         around 1956; it is not an expanding window.
#                         The 12-month predictor lags (01_data_prep.R) leave
#                         ntis lags missing until 1927-12, so the first 12
#                         windows lose up to 12 rows to drop_na() in poos().
#   ROLL_WINDOW = 480  -> e.g. a 40-year window
ROLL_WINDOW  <- NULL
RETUNE_EVERY <- 12                       # re-tune hyperparameters every 12 origins
VAL_MONTHS   <- 240                      # validation block = last 20 years of
                                         # the training window (for tuning)

# Benchmark: prevailing mean (Goyal & Welch 2008) = average of the realised
# equity premia in the same rolling window.

# ---- Trading ----------------------------------------------------------------
# Strategy: every h months, 100% S&P 500 if forecast > 0, else 100% T-bills
# (Rfree). Long-only, no leverage. Start in T-bills.
W0           <- 1e6                      # starting wealth
COST_PER_LEG <- 0.001                    # 0.1% per trade (each buy or sell)
COST         <- 2 * COST_PER_LEG         # a switch = sell one + buy other = 0.2%
SEED  <- 4308
