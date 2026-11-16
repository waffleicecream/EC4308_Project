# =============================================================================
# 19_corr_plot.R  -  Spearman correlation heatmap of the ext predictors
#
# One diagnostic figure for the 30 base ext predictors (their 1, 3 and
# 12-month lags are not drawn). gw is the first 14 of those, so it is not
# drawn separately. Spearman matches the grouping rule in
# R/functions/forest.R (a pair is grouped at 0.7 on the first training
# window) and treats the 0/1 technical signals like the continuous series.
#
# Sample: TRAIN_START through FIRST_ORIGIN (December 1926 to December 1995).
# That is every predictor available at the first test origin. The test
# months are left out.
#
# Order: the IMP_GROUPS blocks (valuation, rates, trend, risk), then the
# predictors in no block, in the order they appear in the ext set.
#
# Pairwise correlation does not display the exact identities. de = dp - ep
# and tms = lty - tbl are linear combinations of two variables, so those
# pairs are strongly correlated and still not equal to 1.
#
# Definitions of the codes on the axes: README.md, section Predictors.
#
# Output: output/figures/predictor_correlation.png
# Run 01_data_prep.R first.
# =============================================================================

source("R/00_config.R")
suppressPackageStartupMessages(library(ggplot2))

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Same theme as 12_performance_plots.R.
BLUE <- "#2a78d6"
INK  <- "#0b0b0b"; INK2 <- "#52514e"; SURF <- "#fcfcfb"
theme_set(theme_minimal(base_size = 11) +
  theme(plot.background  = element_rect(fill = SURF, colour = NA),
        panel.background = element_rect(fill = SURF, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        text = element_text(colour = INK), axis.text = element_text(colour = INK2),
        plot.subtitle = element_text(colour = INK2, size = 9.5),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.justification = "left",
        plot.title.position = "plot"))

# Blocks from IMP_GROUPS in R/functions/forest.R, in that order.
BLOCKS <- list(
  valuation = c("dp", "dy", "ep", "de", "bm"),
  rates     = c("tbl", "lty", "tms"),
  trend     = c("ma_1_9", "ma_1_12", "ma_2_9", "ma_2_12", "ma_3_9", "ma_3_12",
                "mom_9", "mom_12", "dtoy"),
  risk      = c("svar", "avgcor")
)

prep <- readRDS(PROC_FILE)
vars <- prep$sets$ext$base     # base predictors only; the lags are not drawn
ord  <- c(unlist(BLOCKS, use.names = FALSE), setdiff(vars, unlist(BLOCKS)))
stopifnot(setequal(ord, vars), !anyDuplicated(ord), length(ord) == 30)

x <- prep$data |>
  filter(date >= TRAIN_START, date <= FIRST_ORIGIN) |>
  select(all_of(ord))
stopifnot(nrow(x) > 0, !anyNA(x))

C <- cor(x, method = "spearman")
stopifnot(all(abs(diag(C) - 1) < 1e-12), all(!is.na(C)))

long <- as.data.frame(as.table(C), stringsAsFactors = FALSE)
names(long) <- c("x", "y", "rho")
long$x <- factor(long$x, levels = ord)
long$y <- factor(long$y, levels = rev(ord))

g_corr <- ggplot(long, aes(x, y, fill = rho)) +
  geom_tile() +
  scale_fill_gradient2(low = "#a33b32", mid = SURF, high = BLUE,
                       midpoint = 0, limits = c(-1, 1),
                       name = "Spearman") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed() +
  labs(title = "Predictor correlations, ext set",
       subtitle = paste0("Spearman, ", format(TRAIN_START, "%B %Y"), " to ",
                         format(FIRST_ORIGIN, "%B %Y"), ". Definitions in the README."),
       x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.key.width = unit(1.4, "cm"))

ggsave(file.path(FIG_DIR, "predictor_correlation.png"), g_corr,
       width = 8.5, height = 8, dpi = 150)

cat(sprintf("Spearman correlations: %d predictors, %d months (%s to %s)\n",
            ncol(x), nrow(x), min(prep$data$date[prep$data$date <= FIRST_ORIGIN &
                                                   prep$data$date >= TRAIN_START]),
            max(prep$data$date[prep$data$date <= FIRST_ORIGIN &
                                 prep$data$date >= TRAIN_START])))
cat("Saved predictor_correlation.png to", FIG_DIR, "\n")
