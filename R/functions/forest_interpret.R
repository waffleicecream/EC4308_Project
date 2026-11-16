# =============================================================================
# forest_interpret.R  -  variable importance plots for RF and bagging
#
# Uses the importance logged at each annual tuning date (10 dates per
# set x horizon, 2015-12 ... 2024-12), under two methods:
#   method = "oob"  out-of-bag (lecture method), by 03_rf.R / 03_bag.R via
#                   oob_importance() in forest.R
#                   -> output/tables/<model>/<model>_importance.csv, _group_importance.csv
#   method = "val"  validation block, by 14_forest_val_importance.R via
#                   val_importance() in forest.R
#                   -> output/tables/<model>/<model>_val_importance.csv, _val_group_importance.csv
#
# Measure: inc_mse_pct = % rise in MSE (out-of-bag, or on the validation block)
# when the variable or group is shuffled. Per set x horizon: average over the
# 10 tuning dates, then rank. Unlike the XGBoost plot (17_xgb_interpret.R) this
# is not rescaled to 100, because the % rise in error is directly interpretable.
#
# Figures: output/figures/importance_oob/      one method per figure
#          output/figures/importance_val/
#          output/figures/importance_compare/  both methods side by side
#
# Sourced by 15_rf_interpret.R and 16_bag_interpret.R.
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))
source("R/functions/forest_paths.R")   # model_tab_dir()

# ---- Style (same as 17_xgb_interpret.R) ---------------------------------------
BLUE   <- "#2a78d6"
ORANGE <- "#d9822b"
INK  <- "#0b0b0b"; INK2 <- "#52514e"; SURF <- "#fcfcfb"
theme_set(theme_minimal(base_size = 11) +
  theme(plot.background  = element_rect(fill = SURF, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        text = element_text(colour = INK), axis.text = element_text(colour = INK2),
        plot.subtitle = element_text(colour = INK2, size = 9.5),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot"))
h_lab <- function(h) paste0("h = ", h, (if (h == 1) " month" else " months"))
lab1  <- function(x) sub("^-0\\.0$", "0.0", sprintf("%.1f", x))   # no "-0.0"

MODEL_LABEL  <- c(rf = "Random forest", bag = "Bagging",
                  rf_lag  = "Random forest (lagged infl)",
                  bag_lag = "Bagging (lagged infl)")
METHOD_LABEL <- c(oob = "out-of-bag", val = "validation-block")
METHOD_MSE   <- c(oob = "out-of-bag MSE", val = "validation-block MSE (last 240 months)")
TOP_N <- 15     # bars per panel; gw has 14 predictors so shows all

# Common x-axis (% rise in MSE) for every plot of a method, so they compare
# directly. Largest values: oob 67.6 (bagging gw valuation, h = 12);
# val -10.1 to 4.0 (bagging). Negative = the forest forecast the validation
# months better with the variable scrambled.
X_LIM <- list(oob = c(0, 70), val = c(-12, 6), compare = c(-12, 70))
X_BRK <- list(oob = 10, val = 2, compare = 10)

# Table name for a model x kind x method
imp_file <- function(model, kind, method)
  file.path(model_tab_dir(model), sprintf("%s_%s%s.csv", model, if (method == "val") "val_" else "", kind))

# Average over the tuning dates and rank, per set x horizon
summarise_importance <- function(model, kind, method) {
  imp <- read.csv(imp_file(model, kind, method))
  if (kind == "group_importance")
    imp <- imp |> mutate(feature = ifelse(n_vars > 1,
                                          sprintf("%s (%d)", group, n_vars), group))
  imp |>
    group_by(set, h, feature, description) |>
    summarise(mean_pct = mean(inc_mse_pct), n_dates = n(), .groups = "drop") |>
    group_by(set, h) |>
    arrange(set, h, desc(mean_pct)) |>
    mutate(rank = row_number()) |>
    ungroup()
}

fig_dir <- function(sub) {
  d <- file.path(FIG_DIR, sub)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# kind = "importance"        one variable at a time
# kind = "group_importance"  correlated predictors shuffled together (groups in
#                            forest.R), so near-duplicates such as dp/dy/ep
#                            cannot cover for each other. Groups are labelled
#                            "name (n)"; a predictor with no partner is its own
#                            group of one, under its own name.
# top_n: bars per panel. A value other than TOP_N saves a separate figure
# with a "_top<n>" suffix, so the default figures are left as they are.
# x_lim: x-axis range; defaults to the common axis for the method (X_LIM).
plot_forest_importance <- function(model, kind = c("importance", "group_importance"),
                                   method = c("oob", "val"), top_n = TOP_N,
                                   x_lim = NULL) {
  kind <- match.arg(kind); method <- match.arg(method)
  if (is.null(x_lim)) x_lim <- X_LIM[[method]]
  suffix <- if (top_n == TOP_N) "" else paste0("_top", top_n)
  what <- if (kind == "importance") "variable" else "group"
  summ <- summarise_importance(model, kind, method)
  write.csv(summ, sub("\\.csv$", "_summary.csv", imp_file(model, kind, method)),
            row.names = FALSE)
  out_dir <- fig_dir(paste0("importance_", method))

  for (s in unique(summ$set)) {
    ds <- summ |> filter(set == s, rank <= top_n) |>
      mutate(hl  = factor(sapply(h, h_lab), levels = sapply(HORIZONS, h_lab)),
             key = factor(paste(feature, h, sep = "___")))
    ds$key <- reorder(ds$key, ds$mean_pct)          # order bars within panel
    n_vars <- n_distinct(summ$feature[summ$set == s])
    g <- ggplot(ds, aes(mean_pct, key)) +
      geom_col(fill = BLUE, width = 0.7) +
      geom_text(aes(label = lab1(mean_pct),
                    hjust = ifelse(mean_pct < 0, 1.25, -0.25)),
                size = 3, colour = INK2) +
      geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
      facet_wrap(~ hl, scales = "free_y") +
      scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
      scale_x_continuous(limits = x_lim,
                         breaks = seq(x_lim[1], x_lim[2], X_BRK[[method]]),
                         expand = c(0, 0),
                         oob = scales::oob_keep) +   # keep values past the limits
      coord_cartesian(clip = "off") +              # labels may pass the limits
      theme(panel.spacing.x = unit(1.5, "lines"),
            plot.margin = margin(5.5, 20, 5.5, 5.5)) +
      labs(title = sprintf("%s %spermutation importance (%s) - %s predictor set",
                           MODEL_LABEL[[model]],
                           if (kind == "group_importance") "group " else "",
                           METHOD_LABEL[[method]], s),
           subtitle = paste0("% rise in ", METHOD_MSE[[method]], " when the ", what,
                             if (kind == "group_importance") " (n = members)" else "",
                             " is shuffled, ",
                             "averaged over the 10 annual tuning dates",
                             if (n_vars > top_n) sprintf(" | top %d of %d shown", top_n, n_vars)
                             else ""),
           x = paste0("Rise in ", if (method == "oob") "OOB" else "validation", " MSE (%)"),
           y = NULL)
    ggsave(file.path(out_dir, sprintf("%s_%s_%s%s.png", model, kind, s, suffix)), g,
           width = 10, height = if (top_n < 10) 3.5 else if (n_vars > top_n) 5.5 else 5,
           dpi = 150)
  }

  cat("Saved", method, kind, "figures to", out_dir, "\n")
  invisible(summ)
}

# Both methods side by side: bars ordered by the out-of-bag score, so the
# gap shows how much of the out-of-bag importance survives on later months.
plot_importance_compare <- function(model, kind = c("importance", "group_importance")) {
  kind <- match.arg(kind)
  what <- if (kind == "importance") "variable" else "group"
  both <- bind_rows(
    summarise_importance(model, kind, "oob") |> mutate(method = "oob"),
    summarise_importance(model, kind, "val") |> mutate(method = "val"))
  out_dir <- fig_dir("importance_compare")

  for (s in unique(both$set)) {
    keep <- both |> filter(set == s, method == "oob", rank <= TOP_N) |>
      select(h, feature, oob_pct = mean_pct)
    ds <- both |> filter(set == s) |> inner_join(keep, by = c("h", "feature")) |>
      mutate(hl     = factor(sapply(h, h_lab), levels = sapply(HORIZONS, h_lab)),
             key    = reorder(factor(paste(feature, h, sep = "___")), oob_pct),
             method = factor(METHOD_LABEL[method], levels = rev(METHOD_LABEL)))
    n_vars <- n_distinct(both$feature[both$set == s])
    g <- ggplot(ds, aes(mean_pct, key, fill = method)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = lab1(mean_pct),
                    hjust = ifelse(mean_pct < 0, 1.2, -0.2)),
                position = position_dodge(width = 0.8), size = 2.4, colour = INK2) +
      geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
      facet_wrap(~ hl, scales = "free_y") +
      scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
      scale_fill_manual(values = setNames(c(BLUE, ORANGE), METHOD_LABEL),
                        breaks = unname(METHOD_LABEL), name = NULL) +
      scale_x_continuous(limits = X_LIM$compare,
                         breaks = seq(-10, X_LIM$compare[2], X_BRK$compare),
                         expand = c(0, 0), oob = scales::oob_keep) +
      coord_cartesian(clip = "off") +
      theme(panel.spacing.x = unit(1.5, "lines"),
            plot.margin = margin(5.5, 20, 5.5, 5.5),
            legend.position = "top", legend.justification = "left") +
      labs(title = sprintf("%s %spermutation importance: out-of-bag vs validation block - %s predictor set",
                           MODEL_LABEL[[model]],
                           if (kind == "group_importance") "group " else "", s),
           subtitle = paste0("% rise in MSE when the ", what, " is shuffled, ",
                             "averaged over the 10 annual tuning dates; ",
                             "ordered by the out-of-bag score",
                             if (n_vars > TOP_N) sprintf(" | top %d of %d (out-of-bag) shown", TOP_N, n_vars)
                             else ""),
           x = "Rise in MSE (%)", y = NULL)
    ggsave(file.path(out_dir, sprintf("%s_%s_compare_%s.png", model, kind, s)), g,
           width = 11, height = if (n_vars > TOP_N) 7.5 else 6.5, dpi = 150)
  }
  cat("Saved compare", kind, "figures to", out_dir, "\n")
  invisible(both)
}

# Everything for one model: both methods and the comparison, for single
# variables and groups.
plot_forest_all <- function(model) {
  for (k in c("importance", "group_importance")) {
    plot_forest_importance(model, k, "oob")
    if (file.exists(imp_file(model, k, "val"))) {
      plot_forest_importance(model, k, "val")
      plot_importance_compare(model, k)
    } else cat("No", imp_file(model, k, "val"), "- run 14_forest_val_importance.R\n")
  }
}
