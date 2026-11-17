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
MAX_LAG      <- 4                        # lags t-1..t-4 of every predictor.
                                         # t-1 is the value on the origin row
                                         # (the column's own name). t-k is that
                                         # series k-1 months earlier, stored as
                                         # var_l2, var_l3, var_l4.
TRAIN_START  <- as.Date("1927-03-31")    # first month used for training (all
                                         # sets): first month every predictor's
                                         # t-4 lag is available. ntis, the last
                                         # series to start, begins 1926-12, so
                                         # its t-4 lag is first observed here.
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
#                         (starts at TRAIN_START = 1927-03 at the first origin,
#                         825 months for h = 1)
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
