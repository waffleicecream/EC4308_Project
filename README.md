# Forecasting the US equity premium with machine learning

EC4308 group project. We forecast the US log equity premium (S&P 500 total return minus the T-bill rate) 1, 3 and 12 months ahead with ten machine-learning and regularised-regression models and their equal-weighted average. Each model is compared with the historical-mean benchmark of Goyal and Welch (2008) over a 30-year pseudo-out-of-sample test period, January 1996 to December 2025. The comparison uses forecast accuracy and a market-timing trading strategy.

## Quick start

You need R 4.1 or newer. Run every command from the project root, the folder that contains this README.

```bash
Rscript install_packages.R   # once: installs the CRAN packages and HDeconometrics (GitHub)
Rscript run_all.R            # rebuilds every table and figure (about 16 minutes)
```

The main results are then in:

| File | Contents |
|---|---|
| `output/tables/all_eval.csv` | Forecast accuracy of every model and horizon: OOS R², Diebold–Mariano test, hit rate, Pesaran–Timmermann test |
| `output/tables/trading_summary.csv` | Trading results of every strategy: CAGR, Sharpe ratio, maximum drawdown, switches |
| `output/tables/cum_performance_ext.csv` | The two above joined, one row per model and horizon |
| `output/figures/` | All figures |

`run_all.R` does not re-estimate the slowest parts. It reuses their saved results instead:

- **Random forest and bagging** reload their finished runs from `output/checkpoints/`. To re-estimate them from scratch, delete those files first. This takes several hours; see [Slow steps](#slow-steps).
- **XGBoost** uses the saved `output/forecasts/xgb.rds`. Add `--xgb` to re-run it, which also takes hours.
- **RF and bagging validation-block importance** uses the saved tables. Add `--val-importance` to recompute them (about 8 minutes). Because the importance comes from random shuffles, the smallest bars can then change order slightly.

Everything else is recomputed from `data/Data2025.xlsx`.

## Repository layout

```
.
├── run_all.R                  runs the whole pipeline in order
├── install_packages.R         installs the required packages
├── data/
│   ├── Data2025.xlsx          raw data (Goyal–Welch–Zafirov workbook, Monthly sheet)
│   └── processed/             monthly.rds, rebuilt by R/01_data_prep.R (not tracked)
├── R/
│   ├── 00_config.R            settings shared by every script (dates, window, costs, paths)
│   ├── 01_data_prep.R         builds targets, predictors and their lags
│   ├── 02_benchmark.R         historical-mean benchmark
│   ├── models/                03–10: one script per model
│   ├── evaluation/            11–15: ensemble, evaluation table, trading backtest
│   ├── figures/               16–20: plots
│   └── functions/             shared code
│       ├── poos.R             rolling-window pseudo-out-of-sample engine used by every model
│       ├── forecast_utils.R   test origins, training windows, saving forecasts
│       ├── metrics.R          OOS R², Diebold–Mariano, hit rate, Pesaran–Timmermann
│       ├── trading.R          switching backtest and performance summary
│       ├── forest.R           random forest / bagging model, importance, checkpoints
│       ├── forest_interpret.R importance plots for RF and bagging
│       ├── lasso_models.R     LASSO-type models
│       ├── lasso_utils.R      LASSO helpers and parallel runner
│       └── plot_style.R       shared ggplot theme
└── output/
    ├── forecasts/             <model>.rds: one forecast per model, horizon and origin
    ├── tables/                cross-model tables, plus one folder per model
    │   └── <model>/           <model>_eval.csv, tuning logs, coefficients, importance
    ├── figures/
    └── checkpoints/           saved RF and bagging runs (lets run_all.R skip hours of fitting)
```

## Pipeline

The scripts are numbered in the order they must run. `run_all.R` runs them in that order, each in a fresh R process. Any script can also be run on its own, e.g. `Rscript R/models/09_ridge.R`, once the steps before it have run. Times were measured on an Apple M1 Pro laptop (10 cores, 16 GB) with the saved checkpoints.

| Step | Script | What it does | Time |
|---|---|---|---|
| 1 | `R/01_data_prep.R` | Reads the workbook, builds the targets, the 30 predictors and their lags | 1 s |
| 2 | `R/02_benchmark.R` | Historical-mean forecasts | 1 s |
| 3 | `R/models/03_rf.R` | Random forest | 1 s (hours from scratch) |
| 4 | `R/models/04_bag.R` | Bagging | 1 s (hours from scratch) |
| 5 | `R/models/05_xgb.R` | XGBoost (only with `--xgb`) | hours |
| 6 | `R/models/06_lasso.R` | LASSO, λ by BIC | 25 s |
| 7 | `R/models/07_post_lasso.R` | Rigorous LASSO and post-LASSO | 13 min |
| 8 | `R/models/08_pca_lasso.R` | PCA + LASSO and PCR with rigorous-LASSO selection | 40 s |
| 9 | `R/models/09_ridge.R` | Ridge | 45 s |
| 10 | `R/models/10_kitchen_sink.R` | OLS on all predictors | 10 s |
| 11 | `R/evaluation/11_ensemble.R` | Equal-weighted mean of the ten models | 1 s |
| 12 | `R/evaluation/12_all_eval.R` | Stacks every model's evaluation into `all_eval.csv` | 1 s |
| 13 | `R/evaluation/13_trading.R` | Trading backtest for every forecast | 1 s |
| 14 | `R/evaluation/14_forest_trading_tuned.R` | RF and bagging trading results with their tuned settings | 1 s |
| 15 | `R/evaluation/15_forest_val_importance.R` | RF and bagging importance on the validation block (only with `--val-importance`) | 8 min |
| 16 | `R/figures/16_performance_plots.R` | Target over time, cumulative trading performance | 3 s |
| 17 | `R/figures/17_forecast_plots.R` | Predicted vs actual premium for each model | 4 s |
| 18 | `R/figures/18_forest_importance_plots.R` | RF and bagging importance plots | 5 s |
| 19 | `R/figures/19_xgb_interpret.R` | XGBoost importance and partial dependence (needs the `--xgb` logs, see below) | — |
| 20 | `R/figures/20_corr_plot.R` | Predictor correlation heatmap | 1 s |

## Design

**Sample and targets.** Monthly data from December 1926, the first month with every predictor, to December 2025. At each month-end *t* the target for horizon *h* is the log equity premium summed over months *t*+1 to *t*+*h*. Every predictor on row *t* is known at the end of month *t*, so no script shifts the data again.

**Predictors (`ext` set).** 30 base predictors: the 14 monthly series of Goyal and Welch (2008), 8 later series from Goyal, Welch and Zafirov (2024), and 8 technical indicators from Neely, Rapach, Tu and Zhou (2014). Each enters with its current value and its values 1, 3 and 12 months earlier, giving 120 columns. The lags match the three forecast horizons (a month, a quarter, a year). A design with consecutive lags 1–4 was tried as a robustness check on the `cq` branch and is not part of this pipeline. Definitions are in [Predictors](#predictors).

**Pseudo-out-of-sample scheme** (`R/functions/poos.R`). The first forecast origin is December 1995. At each origin the model is trained on a **rolling window** of fixed length, all data before the test period (828 months for *h* = 1). Only rows whose *h*-month target is already realised are used. The window moves forward one month per origin; it does not expand.

- **Tuning.** Every 12 origins, hyperparameters are re-chosen on a validation block: the last 240 months of the window, separated from the fit block by an *h* − 1 month gap so targets do not overlap.
- **Refitting.** The model is refit on the whole window at every origin, giving 360, 358 and 349 forecasts for *h* = 1, 3 and 12.
- **Benchmark.** The historical mean of the target over the same window.

**Models**

| Model | Script | Hyperparameters |
|---|---|---|
| `rf` random forest | 03 | `mtry` ∈ {p/6, p/3, p/2} × `nodesize` ∈ {5, 20, 100, 250, 500}, 500 trees; tuned on the validation block |
| `bag` bagging | 04 | `mtry` = p, `nodesize` as RF |
| `xgb` XGBoost | 05 | depth 1–5 and number of trees (early stopping, up to 5000); learning rate 0.01, subsample 0.5 fixed |
| `lasso_bic` LASSO | 06 | λ by BIC in every window (`HDeconometrics::ic.glmnet`) |
| `rlasso` rigorous LASSO | 07 | plug-in penalty (`hdm::rlasso`), Belloni, Chernozhukov and Hansen (2012) |
| `post_lasso` post-LASSO | 07 | rigorous-LASSO selection, then OLS on the selected predictors |
| `pca_lasso` | 08 | predictors plus their first 4 principal components, LASSO by BIC |
| `pcr_rlasso` | 08 | all principal components, selected by rigorous LASSO, then OLS |
| `ridge` | 09 | λ on an 81-point grid from 10⁻⁶ to 10², tuned on the validation block |
| `kitchen_sink` OLS | 10 | none; drops `de`, `tms` and their lags (exact linear combinations of other predictors) |
| `ensemble_mean` | 11 | none; simple mean of the ten models above |

PCA is fitted inside each training window, so the components never use data from after the origin.

**Evaluation** (`R/functions/metrics.R`)

- **OOS R²** (Campbell and Thompson 2008): 1 − SSE(model) / SSE(historical mean), in %. A positive value means the model beat the benchmark.
- **Diebold–Mariano test.** One-sided, with the Harvey–Leybourne–Newbold correction and *h* − 1 lags for the overlapping targets.
- **Hit rate.** The share of forecasts with the correct sign, tested against chance with the Pesaran–Timmermann test.

**Trading** (`R/functions/trading.R`). Every *h* months, the strategy holds the S&P 500 for the next *h* months if the forecast is positive and T-bills otherwise. It starts with $1m in T-bills, pays 0.1% per buy or sell (0.2% per switch), and is long-only with no leverage. The reference strategies are buy-and-hold, always-T-bills and perfect foresight.

## Checking your run

A complete run ends with `All steps done`. Your `output/tables/all_eval.csv` should then contain, among others, these out-of-sample R² values (%, against the historical mean):

| Model | h = 1 | h = 3 | h = 12 |
|---|---|---|---|
| `ensemble_mean` | 0.611 | −1.363 | −23.584 |
| `xgb` | −2.511 | −0.140 | −4.107 |
| `ridge` | −0.291 | −0.406 | −0.562 |
| `lasso_bic` | 0.002 | −1.544 | −54.198 |
| `rf` | −6.686 | −6.723 | −8.324 |

Every forecast file in `output/forecasts/` reproduces exactly. A few tables can differ from the committed ones without changing any result:

- **Coefficient tables of `pca_lasso` and `pcr_rlasso`:** a principal component's sign is arbitrary and depends on the linear-algebra library, so a coefficient on a component can flip sign. Its absolute value and the forecasts are unchanged.
- **CSV files in general:** the last of 15 printed digits can differ between machines.

Run `git diff --stat` to see which files changed.

## Slow steps

**Random forest and bagging.** `03_rf.R` and `04_bag.R` run the three horizons in parallel (`N_WORKERS` in `R/functions/forest.R`). Each finished horizon is saved to `output/checkpoints/<model>_ext_h<h>.rds`. A checkpoint is reused only if the forest settings it was made with still match. A run that stops part-way therefore resumes where it left off, and changing a setting such as `NODESIZES` forces a refit. Progress is written to `output/checkpoints/forest_progress.log`. From scratch, expect several hours on a laptop for each model, with bagging the slower. On a Slurm cluster, run `Rscript R/models/03_rf.R` and `Rscript R/models/04_bag.R` in a job with at least 3 CPUs, then copy `output/` back.

**XGBoost.** `05_xgb.R` has no checkpoints and takes hours. Only its forecasts, evaluation and tuning log are committed. The importance and partial-dependence logs that `19_xgb_interpret.R` plots are therefore not in the repository: run `Rscript run_all.R --xgb`, or `R/models/05_xgb.R` followed by `R/figures/19_xgb_interpret.R`, to produce them. On a different machine or `xgboost` version the re-run forecasts may differ slightly, and the ensemble changes with them.

**Changing the design.** All dates, the window, the tuning frequency and the trading costs are set in `R/00_config.R`. After changing any of them, delete `output/checkpoints/` and run `Rscript run_all.R --xgb --val-importance`.

## Predictors

Each row of the processed data is a month-end. The 14 Goyal–Welch predictors come first. Their other three series (`eqis`, `i/k`, `cay`) are annual or quarterly and are not used.

A lag is named `<code>_l<k>`, the series *k* months earlier (`dp_l12`). Inflation enters only as `infl_l1`, last month's value, because the CPI for month *t* is published in month *t*+1. Its lags are therefore `infl_l2`, `infl_l4` and `infl_l13`. The lags are built on the full history before the sample is cut at December 1926.

The kitchen sink drops `de`, `tms` and their lags, because `de = dp − ep` and `tms = lty − tbl`. Every other model uses all 120 columns. `R/01_data_prep.R` lists the 14 workbook series that are not used and why (`DROPPED`): look-ahead bias, or too short a history.

| Code | In this repo | Meaning |
|---|---|---|
| `dp` | `log(d12) - log(price)` | Log dividend–price ratio. `d12` is the 12-month sum of S&P 500 dividends (Goyal and Welch 2008). |
| `dy` | `log(d12) - log(lag(price))` | Log dividend yield: the same dividend sum over the previous month's index. |
| `ep` | `log(e12) - log(price)` | Log earnings–price ratio. `e12` is the 12-month sum of S&P 500 earnings. |
| `de` | `log(d12) - log(e12)` | Log dividend payout; equals `dp − ep`. |
| `svar` | sheet column | Sum of squared daily S&P 500 returns in the month. |
| `bm` | sheet column `b/m` | Book-to-market ratio of the Dow Jones Industrial Average. |
| `ntis` | sheet column | Net equity expansion of NYSE stocks. Starts December 1926, which is why the sample does. |
| `tbl` | sheet column | 3-month Treasury-bill yield. |
| `lty` | sheet column | Long-term government bond yield. |
| `ltr` | sheet column | Return on long-term government bonds. |
| `tms` | `lty - tbl` | Term spread. |
| `dfy` | `BAA - AAA` | Default yield spread. |
| `dfr` | `corpr - ltr` | Default return spread: corporate minus government long-term bond return. |
| `infl` | sheet column | CPI inflation. Used only as `infl_l1` (one-month publication lag). |
| `lzrt` | sheet column | Log zero-return frequency, an illiquidity measure (Chen, Eaton and Paye 2018). |
| `wtexas` | sheet column | Monthly change in the West Texas Intermediate oil price (Driesprong, Jacobsen and Maat 2008). |
| `skvw` | sheet column | Average firm-level return skewness (Jondeau, Zhang and Zhu 2019). |
| `tail` | sheet column | Cross-sectional tail risk (Kelly and Jiang 2014). |
| `rdsp` | sheet column | Cross-sectional dispersion of returns on 100 size and book-to-market portfolios (Maio 2016). |
| `dtoy` | sheet column | Dow Jones index over its 52-week high (Li and Yu 2012). |
| `dtoat` | sheet column | Dow Jones index over its all-time high (Li and Yu 2012). |
| `avgcor` | sheet column | Average pairwise correlation of daily returns among the 500 largest stocks (Pollet and Wilson 2010). |
| `ma_1_9`, `ma_1_12` | price ≥ its 9- / 12-month mean | Moving-average signals, 1 = bullish (Neely, Rapach, Tu and Zhou 2014). |
| `ma_2_9`, `ma_2_12` | 2-month mean ≥ 9- / 12-month mean | Moving-average signals. |
| `ma_3_9`, `ma_3_12` | 3-month mean ≥ 9- / 12-month mean | Moving-average signals. |
| `mom_9`, `mom_12` | price ≥ price 9 / 12 months earlier | Momentum signals. |

The workbook's `tchi` is the first principal component of all 14 Neely et al. indicators, with loadings fit on the full sample (look-ahead). It is dropped. The eight price-based indicators are rebuilt instead; the six volume-based ones need volume data the workbook does not have.

## References

- Belloni, A., Chernozhukov, V. and Hansen, C. (2012). Sparse models and methods for optimal instruments with an application to eminent domain. *Econometrica*.
- Campbell, J. Y. and Thompson, S. B. (2008). Predicting excess stock returns out of sample: Can anything beat the historical average? *Review of Financial Studies*.
- Goyal, A. and Welch, I. (2008). A comprehensive look at the empirical performance of equity premium prediction. *Review of Financial Studies*.
- Goyal, A., Welch, I. and Zafirov, A. (2024). A comprehensive 2022 look at the empirical performance of equity premium prediction. *Review of Financial Studies*.
- Medeiros, M. C., Vasconcelos, G. F. R., Veiga, Á. and Zilberman, E. (2019). Forecasting inflation in a data-rich environment: The benefits of machine learning methods. *Journal of Business & Economic Statistics*.
- Neely, C. J., Rapach, D. E., Tu, J. and Zhou, G. (2014). Forecasting the equity risk premium: The role of technical indicators. *Management Science*.
