# Colours and ggplot theme shared by every figure script.

suppressPackageStartupMessages(library(ggplot2))

BLUE   <- "#2a78d6"
ORANGE <- "#d9822b"
GREY   <- "#8a8984"
INK    <- "#0b0b0b"
INK2   <- "#52514e"
SURF   <- "#fcfcfb"

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

h_lab    <- function(h) paste0("h = ", h, (if (h == 1) " month" else " months"))
h_factor <- function(h) factor(sapply(h, h_lab), levels = sapply(HORIZONS, h_lab))

fig_dir <- function(sub = NULL) {
  d <- if (is.null(sub)) FIG_DIR else file.path(FIG_DIR, sub)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}
