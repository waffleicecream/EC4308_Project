# =============================================================================
# 10_trading.R  -  trading backtest for every saved forecast
#
# Reads all output/forecasts/*.rds (benchmark + models), runs the S&P/T-bill
# switching strategy for each model x set x horizon, and adds reference
# strategies: buy-and-hold, always T-bills, perfect foresight (upper bound).
#
# Outputs: output/tables/trading_summary.csv   one row per strategy
#          output/tables/trading_paths.rds     monthly wealth paths (for plots)
# =============================================================================

source("R/00_config.R")
source("R/functions/forecast_utils.R")
source("R/functions/trading.R")

d  <- readRDS(PROC_FILE)$data
fc <- bind_rows(lapply(list.files(FCST_DIR, "\\.rds$", full.names = TRUE), readRDS))

paths <- list()
add <- function(name, set, h, p) {
  paths[[paste(name, set, h)]] <<- p |> mutate(model = name, set = set, h = h)
}

for (hh in HORIZONS) {
  fh  <- fc |> filter(h == hh)
  dec <- sort(unique(fh$date))
  dec <- dec[seq(1, length(dec), by = hh)]          # decide every hh months

  # forecast-based strategies
  for (g in split(fh, list(fh$model, fh$set), drop = TRUE)) {
    g   <- g[match(dec, g$date), ]
    stopifnot(!anyNA(g$yhat))
    add(g$model[1], g$set[1], hh, backtest_switch(d, dec, g$yhat > 0, hh))
  }

  # reference strategies
  yreal <- d[[target_col(hh)]][match(dec, d$date)]
  add("buy_hold",          "-", hh, backtest_switch(d, dec, rep(1, length(dec)), hh))
  add("tbill",             "-", hh, backtest_switch(d, dec, rep(0, length(dec)), hh))
  add("perfect_foresight", "-", hh, backtest_switch(d, dec, yreal > 0, hh))
}

# Sanity check: buy-and-hold = one entry cost, then compounded S&P returns
m  <- test_months(d)
bh <- paths[["buy_hold - 1"]]
stopifnot(isTRUE(all.equal(tail(bh$wealth, 1),
                           W0 * (1 - COST) * prod(1 + d$ret_next[m]))))

summ <- bind_rows(lapply(paths, function(p)
  bind_cols(p[1, c("model", "set", "h")], perf_summary(p)))) |>
  arrange(h, desc(sharpe)) |>
  mutate(across(where(is.double) & !c(h), \(x) round(x, 3)))

dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(summ, file.path(TAB_DIR, "trading_summary.csv"), row.names = FALSE)
saveRDS(bind_rows(paths), file.path(TAB_DIR, "trading_paths.rds"))

cat(sprintf("Test period: %s to %s (%d months), start $%s, cost %.1f%% per switch (0.1%% per leg)\n",
            format(min(bh$date)), format(max(bh$date)), nrow(bh),
            format(W0, big.mark = ",", scientific = FALSE), 100 * COST))
print(summ |> mutate(final_wealth = round(final_wealth / 1e6, 2)) |>
        rename(`final_$m` = final_wealth), n = Inf, width = Inf)
