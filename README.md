# Predictors

`R/01_data_prep.R` builds the monthly data from `data/Data2025.xlsx`. Each row is a month-end. The predictors on that row are known at that date, and later scripts do not shift them again.

Two sets:

- **gw** — the 14 monthly predictors from Goyal and Welch (2008). Their other three series (`eqis`, `i/k`, `cay`) are annual or quarterly and are in neither set.
- **ext** — those 14, the eight later monthly series below, and the eight technical indicators. Thirty predictors in all.

Goyal and Welch (2008), "A Comprehensive Look at the Empirical Performance of Equity Premium Prediction," *Review of Financial Studies*, define the original 14. Goyal, Welch, and Zafirov (2024), "A Comprehensive 2022 Look at the Empirical Performance of Equity Premium Prediction," *Review of Financial Studies*, use those definitions and add the later series. The workbook carries the series through 2025.

`de` equals `dp − ep`, and `tms` equals `lty − tbl`. Each identity uses two variables, so the pairwise correlation of either pair is high and is not 1.

## The 30 `ext` predictors

`gw` is the first block. The newer series are copied from the Monthly sheet. The technical indicators are computed in `R/01_data_prep.R`.

| Code | In this repo | Meaning |
|---|---|---|
| `dp` | `log(d12) - log(price)` | Log dividend–price ratio. `d12` is the 12-month sum of dividends on the S&P 500, and `price` is the index (Goyal and Welch 2008). |
| `dy` | `log(d12) - log(lag(price))` | Log dividend yield: the same dividend sum over the previous month's index (Goyal and Welch 2008). |
| `ep` | `log(e12) - log(price)` | Log earnings–price ratio. `e12` is the 12-month sum of S&P 500 earnings (Goyal and Welch 2008). |
| `de` | `log(d12) - log(e12)` | Log dividend payout. This equals `dp − ep` (Goyal and Welch 2008). |
| `svar` | Monthly-sheet column | Sum of squared daily S&P 500 returns in the month (Goyal and Welch 2008). |
| `bm` | Sheet column `b/m` | Book-to-market ratio of the Dow Jones Industrial Average. March–December uses the previous year-end book value; January and February use the book value from two years earlier (Goyal and Welch 2008). |
| `ntis` | Monthly-sheet column | Net equity expansion: the 12-month sum of net issues by NYSE stocks, divided by the year-end NYSE market capitalization (Goyal and Welch 2008). This series starts in December 1926, which is why the sample does. |
| `tbl` | Monthly-sheet column | Annualized 3-month Treasury-bill yield. Goyal, Welch, and Zafirov (2024) call the same series `tby` (Goyal and Welch 2008). |
| `lty` | Monthly-sheet column | Annualized long-term government bond yield (Goyal and Welch 2008). |
| `ltr` | Monthly-sheet column | Monthly return on long-term government bonds (Goyal and Welch 2008). |
| `tms` | `lty - tbl` | Term spread, the long-term yield minus the Treasury-bill yield (Goyal and Welch 2008). |
| `dfy` | `BAA - AAA` | Default yield spread: the Moody's BAA corporate yield minus the AAA yield (Goyal and Welch 2008). |
| `dfr` | `corpr - ltr` | Default return spread: the monthly long-term corporate bond return minus the long-term government bond return (Goyal and Welch 2008). |
| `infl` | Monthly-sheet column | Same-month CPI inflation, (CPIₜ − CPIₜ₋₁) / CPIₜ₋₁. Goyal and Welch (2008) lag this by one month in their regressions because the CPI is released the next month. In this repo that lag is a separate series, `infl_l1`, used only by the `rf_lag` and `bag_lag` runs. |
| `lzrt` | Monthly-sheet column | Log of the zero-return frequency (Chen, Eaton, and Paye 2018). Goyal, Welch, and Zafirov (2024, Section 3.1.4) describe a version residualized on the 1997 and 2001 tick-size breaks. The column here is the log frequency as posted: it lies between about −2.8 and −1.0. |
| `wtexas` | Monthly-sheet column | Monthly change in the West Texas Intermediate crude-oil price. Goyal, Welch, and Zafirov (2024) extend it back before the 1973 start in Driesprong, Jacobsen, and Maat (2008). |
| `skvw` | Monthly-sheet column | Average, across firms, of each firm's monthly return skewness (Jondeau, Zhang, and Zhu 2019). |
| `tail` | Monthly-sheet column | Tail risk estimated from firm-level crashes in the cross-section of returns (Kelly and Jiang 2014). |
| `rdsp` | Monthly-sheet column | Cross-sectional standard deviation of returns on the 100 size and book-to-market portfolios (Maio 2016). |
| `dtoy` | Monthly-sheet column | Dow Jones index divided by its 52-week high. The column is a nearness ratio on (0, 1] and equals 1 at the high (Li and Yu 2012). |
| `dtoat` | Monthly-sheet column | Dow Jones index divided by its running all-time high, on the same (0, 1] scale (Li and Yu 2012). |
| `avgcor` | Monthly-sheet column | Average correlation of daily returns among the 500 largest stocks, weighting each pair by the product of the two stocks' market-cap weights (Pollet and Wilson 2010). |
| `ma_1_9` | `price` ≥ its 9-month trailing mean | Moving-average signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `ma_1_12` | `price` ≥ its 12-month trailing mean | Moving-average signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `ma_2_9` | 2-month mean ≥ 9-month mean | Moving-average signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `ma_2_12` | 2-month mean ≥ 12-month mean | Moving-average signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `ma_3_9` | 3-month mean ≥ 9-month mean | Moving-average signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `ma_3_12` | 3-month mean ≥ 12-month mean | Moving-average signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `mom_9` | `price` ≥ `price` lagged 9 months | Momentum signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |
| `mom_12` | `price` ≥ `price` lagged 12 months | Momentum signal, 1 when the inequality holds (Neely, Rapach, Tu, and Zhou 2014). |

A trailing mean of length k is the mean of the current month and the previous k − 1 months. The workbook's `tchi` is the first principal component of all 14 Neely, Rapach, Tu, and Zhou indicators, including six on-balance-volume rules, with loadings fit on the full sample. `R/01_data_prep.R` drops it. The six volume rules need volume, which the workbook does not have.
