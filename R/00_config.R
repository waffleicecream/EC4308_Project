# Settings shared by every script. Run all scripts from the project root.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# ---- Paths ------------------------------------------------------------------
RAW_FILE  <- "data/Data2025.xlsx"
PROC_FILE <- "data/processed/monthly.rds"
FCST_DIR  <- "output/forecasts"
TAB_DIR   <- "output/tables"
FIG_DIR   <- "output/figures"
CKPT_DIR  <- "output/checkpoints"

# ---- Forecasting design -----------------------------------------------------
HORIZONS     <- c(1, 3, 12)              # months ahead; also the trading rebalance frequency
TRAIN_START  <- as.Date("1926-12-31")    # first month with every predictor (ntis starts here)
TEST_YEARS   <- 30
TEST_END     <- as.Date("2025-12-31")    # last month whose return is forecast
month_end    <- function(d) floor_date(d, "month") %m+% months(1) - days(1)
TEST_START   <- month_end(TEST_END %m-% months(12 * TEST_YEARS - 1))  # 1996-01-31
FIRST_ORIGIN <- month_end(floor_date(TEST_START, "month") - days(1))  # 1995-12-31

# Rolling window. NULL = a fixed length equal to all data before the test
# period (828 months for h = 1: a 588-month fit block and a 240-month
# validation block). The window rolls forward one month per origin; it does
# not expand. Set a number (e.g. 480) for a shorter window.
ROLL_WINDOW  <- NULL
RETUNE_EVERY <- 12       # re-tune hyperparameters every 12 origins
VAL_MONTHS   <- 240      # validation block: last 20 years of the training window

# ---- Trading ----------------------------------------------------------------
W0           <- 1e6                  # starting wealth
COST_PER_LEG <- 0.001                # 0.1% per buy or sell
COST         <- 2 * COST_PER_LEG     # a switch sells one asset and buys the other

SEED <- 4308
