# Spearman correlation heatmap of the 30 base predictors (lags not drawn),
# December 1926 to December 1995, i.e. the data before the test period.
# Ordered by the importance groups in R/functions/forest.R, then the rest.
# de = dp - ep and tms = lty - tbl are combinations of two variables, so
# those pairs are highly correlated but not equal to 1.
#
# Output: output/figures/predictor_correlation.png

source("R/00_config.R")
source("R/functions/plot_style.R")
source("R/functions/forest.R")      # IMP_GROUPS

prep <- readRDS(PROC_FILE)
vars <- prep$sets$ext$base
ord  <- c(unlist(IMP_GROUPS, use.names = FALSE), setdiff(vars, unlist(IMP_GROUPS)))
stopifnot(setequal(ord, vars), !anyDuplicated(ord))

x <- prep$data |>
  filter(date >= TRAIN_START, date <= FIRST_ORIGIN) |>
  select(all_of(ord))
stopifnot(nrow(x) > 0, !anyNA(x))

C <- cor(x, method = "spearman")
long <- as.data.frame(as.table(C), stringsAsFactors = FALSE)
names(long) <- c("x", "y", "rho")
long$x <- factor(long$x, levels = ord)
long$y <- factor(long$y, levels = rev(ord))

g_corr <- ggplot(long, aes(x, y, fill = rho)) +
  geom_tile() +
  scale_fill_gradient2(low = "#a33b32", mid = SURF, high = BLUE,
                       midpoint = 0, limits = c(-1, 1), name = "Spearman") +
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

ggsave(file.path(fig_dir(), "predictor_correlation.png"), g_corr,
       width = 8.5, height = 8, dpi = 150)
cat(sprintf("Spearman correlations: %d predictors, %d months. Saved to %s\n",
            ncol(x), nrow(x), FIG_DIR))
