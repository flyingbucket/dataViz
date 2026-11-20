#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# Data visualisation assignment 3
# Usage:
#   Rscript Plot2311067.R <input data file> <output image file>
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop("Usage: Rscript Plot2311067.R <input data file> <output image file>",
       call. = FALSE)
}

input_file  <- args[1]
output_file <- args[2]

if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file, call. = FALSE)
}

# Helpers

font_family <- "Liberation Sans"

read_input <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("tab", "tsv", "txt")) {
    utils::read.delim(path, header = TRUE, stringsAsFactors = FALSE,
                      check.names = FALSE, comment.char = "")
  } else {
    utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE,
                    check.names = FALSE)
  }
}

save_plot <- function(plot, path, width, height, dpi = 300) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height,
                  dpi = dpi, units = "in", bg = "white")
}

dat <- read_input(input_file)
names_lower <- tolower(names(dat))

# histogram (Histogram.csv)

plot_histogram <- function(dat, output_file) {
  dat <- dat[, c("eruptions", "waiting")]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]

  long <- data.frame(
    variable = factor(rep(c("eruptions", "waiting"), each = nrow(dat)),
                      levels = c("eruptions", "waiting")),
    value    = c(dat$eruptions, dat$waiting)
  )

  pal <- c(eruptions = "#87CEEB", waiting = "#80CDC1")

  p <- ggplot(long, aes(x = value, fill = variable)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20,
                   colour = "white", linewidth = 0.55) +
    geom_density(aes(y = after_stat(density)), colour = "red",
                 fill = NA, linewidth = 1.0, adjust = 1,
                 show.legend = FALSE) +
    facet_wrap(~variable, scales = "free") +
    scale_fill_manual(values = pal) +
    labs(x = "Value", y = "Density", fill = "variable") +
    theme_bw(base_family = font_family, base_size = 20) +
    theme(
      text              = element_text(family = font_family),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_blank(),
      panel.grid.major  = element_line(colour = "#EBEBEB", linewidth = 0.40),
      panel.grid.minor  = element_line(colour = "#EBEBEB", linewidth = 0.22),
      axis.line         = element_line(colour = "black", linewidth = 1.10),
      axis.ticks        = element_line(colour = "black", linewidth = 1.10),
      axis.ticks.length = unit(3.0, "pt"),
      axis.text         = element_text(colour = "#4D4D4D", face = "bold",
                                       size = 20),
      axis.title        = element_text(colour = "black", face = "bold",
                                       size = 28),
      axis.title.x      = element_text(margin = margin(t = 12)),
      axis.title.y      = element_text(margin = margin(r = 16)),
      strip.background  = element_blank(),
      strip.text        = element_text(colour = "black", face = "bold",
                                       size = 27, margin = margin(b = 12)),
      panel.spacing     = unit(2.4, "cm"),
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.title      = element_text(colour = "black", face = "bold",
                                       size = 21),
      legend.text       = element_text(colour = "black", face = "bold",
                                       size = 21),
      legend.key.size   = unit(30, "pt"),
      legend.key.width  = unit(30, "pt"),
      legend.spacing.x  = unit(0.55, "cm"),
      legend.margin     = margin(0, 0, 0, 0),
      plot.margin       = margin(8, 14, 8, 8)
    )

  save_plot(p, output_file, width = 3543 / 300, height = 1771 / 300)
}

# bars with error bars and p-values (Barplot.tab)

plot_barplot <- function(dat, output_file) {
  idx <- match(c("column", "method", "tmscore"), tolower(names(dat)))
  dat <- data.frame(setting = dat[[idx[1]]],
                    method  = dat[[idx[2]]],
                    score   = as.numeric(dat[[idx[3]]]))

  dat$setting <- factor(dat$setting, levels = unique(dat$setting))
  dat$method  <- factor(dat$method, levels = unique(dat$method))

  agg <- stats::aggregate(score ~ setting + method, data = dat,
                          FUN = function(x) {
                            c(mean = mean(x), se = stats::sd(x) / sqrt(length(x)))
                          })
  agg <- data.frame(setting = agg$setting, method = agg$method,
                    mean = agg$score[, "mean"], se = agg$score[, "se"])

  # p-values shown in the reference figure
  pvals <- c("Setting A" = "0.00039",
             "Setting B" = "5.9e-07",
             "Setting C" = "0.00014",
             "Setting D" = "1.3e-15")

  ann <- do.call(rbind, lapply(levels(dat$setting), function(s) {
    p <- tryCatch(
      stats::wilcox.test(score ~ method, data = subset(dat, setting == s))$p.value,
      error = function(e) NA_real_
    )
    txt <- if (!is.null(pvals[[s]])) pvals[[s]] else format(signif(p, 2), scientific = TRUE)
    data.frame(setting = s, label = paste0("p-value = ", txt))
  }))
  ann$setting <- factor(ann$setting, levels = levels(dat$setting))

  br <- data.frame(x = c(1, 1, 2), xend = c(2, 1, 2),
                   y = c(594, 571, 571), yend = c(594, 594, 594))

  p <- ggplot(agg, aes(x = method, y = mean, fill = method)) +
    geom_col(width = 0.70, colour = "black", linewidth = 0.80) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.32, linewidth = 0.80) +
    geom_segment(data = br,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE, linewidth = 0.60) +
    geom_text(data = ann, aes(x = 1.5, y = 610, label = label),
              inherit.aes = FALSE, size = 9.4, colour = "black") +
    facet_wrap(~setting, nrow = 1) +
    scale_fill_manual(values = c(ModelX = "#8ECFC9", ModelY = "#FFBE7A")) +
    scale_y_continuous(breaks = seq(100, 600, 100),
                       minor_breaks = seq(0, 700, 50)) +
    coord_cartesian(ylim = c(92, 648), expand = FALSE) +
    labs(y = "TM-score", x = NULL) +
    theme_bw(base_family = font_family, base_size = 28) +
    theme(
      text              = element_text(family = font_family),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(colour = "black", fill = NA,
                                       linewidth = 0.75),
      panel.grid.major  = element_line(colour = "#BEBEBE", linewidth = 0.45,
                                       linetype = "dashed"),
      panel.grid.minor  = element_line(colour = "#BEBEBE", linewidth = 0.25,
                                       linetype = "dashed"),
      axis.text         = element_text(colour = "black", size = 28),
      axis.title.y      = element_text(colour = "black", face = "bold",
                                       size = 29, margin = margin(r = 14)),
      axis.title.x      = element_blank(),
      axis.ticks        = element_line(colour = "black", linewidth = 0.70),
      axis.ticks.length = unit(9.0, "pt"),
      strip.background  = element_rect(fill = "#E5E5E5", colour = "black",
                                       linewidth = 0.75),
      strip.text        = element_text(colour = "black", face = "bold",
                                       size = 23,
                                       margin = margin(4, 0, 4, 0)),
      panel.spacing     = unit(0.45, "cm"),
      legend.position   = "none",
      plot.margin       = margin(0.45, 0.45, 0.45, 0.45, "cm")
    )

  save_plot(p, output_file, width = 16, height = 8, dpi = 320)
}

# circular bar plot (CircularBar.csv)

plot_circular <- function(dat, output_file) {
  idx <- match(c("group", "individual", "value"), tolower(names(dat)))
  dat <- data.frame(group      = dat[[idx[1]]],
                    individual = dat[[idx[2]]],
                    value      = as.numeric(dat[[idx[3]]]))

  dat$group      <- factor(dat$group, levels = unique(dat$group))
  dat$individual <- factor(dat$individual, levels = unique(dat$individual))

  gap <- 3L
  dat$id <- NA_integer_
  pos <- 1L
  for (g in levels(dat$group)) {
    k <- sum(dat$group == g)
    dat$id[dat$group == g] <- pos:(pos + k - 1L)
    pos <- pos + k + gap
  }
  n_slots <- max(dat$id) + gap

  dat$angle <- 90 - 360 * (dat$id - 0.5) / n_slots
  dat$hjust <- ifelse(dat$angle < -90, 1, 0)
  dat$angle <- ifelse(dat$angle < -90, dat$angle + 180, dat$angle)

  cols <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")
  cols <- grDevices::adjustcolor(cols[seq_len(nlevels(dat$group))], alpha.f = 0.5)

  p <- ggplot(dat, aes(x = id, y = value, fill = group)) +
    geom_col(width = 0.90, colour = "white", linewidth = 0.25) +
    geom_text(aes(label = individual, y = value + 2.5,
                  angle = angle, hjust = hjust),
              colour = "#4D4D4D", size = 4.0, family = font_family) +
    scale_fill_manual(values = cols, name = "group") +
    scale_x_continuous(limits = c(0.5, n_slots + 0.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-100, 125), expand = c(0, 0)) +
    coord_polar(start = 0, clip = "off") +
    theme_void(base_family = font_family) +
    theme(
      text            = element_text(family = font_family),
      legend.position = "right",
      legend.title    = element_text(colour = "black", face = "bold",
                                     size = 20),
      legend.text     = element_text(colour = "black", face = "bold",
                                     size = 17),
      legend.key.size = unit(16, "pt"),
      legend.key      = element_rect(fill = NA, colour = NA),
      plot.margin     = margin(0.25, 0.25, 0.25, 0.25, "cm")
    )

  save_plot(p, output_file, width = 2362 / 300, height = 1771 / 300)
}

# connected scatter plot (ConnectedScatter.csv)

plot_connected <- function(dat, output_file) {
  idx <- match(c("feature", "auc", "mcc"), tolower(names(dat)))
  dat <- data.frame(feature = dat[[idx[1]]],
                    AUC     = as.numeric(dat[[idx[2]]]),
                    MCC     = as.numeric(dat[[idx[3]]]))
  dat$feature <- factor(dat$feature, levels = unique(dat$feature))

  long <- rbind(
    data.frame(feature = dat$feature, series = "AUC", value = dat$AUC),
    data.frame(feature = dat$feature, series = "MCC", value = dat$MCC)
  )
  long$series <- factor(long$series, levels = c("AUC", "MCC"))

  p <- ggplot(long, aes(x = feature, y = value,
                        colour = series, linetype = series,
                        shape = series, group = series)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 3.4) +
    scale_colour_manual(values = c(AUC = "#CC66CC", MCC = "#66B2FF")) +
    scale_linetype_manual(values = c(AUC = "solid", MCC = "dotted")) +
    scale_shape_manual(values = c(AUC = 16, MCC = 17)) +
    scale_x_discrete(expand = expansion(add = 0.19)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       limits = c(0, 1),
                       expand = expansion(mult = 0.05),
                       labels = c("0", "0.2", "0.4", "0.6", "0.8", "1")) +
    labs(x = "Feature combination", y = "Value",
         colour = NULL, linetype = NULL, shape = NULL) +
    theme_bw(base_family = font_family, base_size = 26) +
    theme(
      text              = element_text(family = font_family),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(colour = "black", fill = NA,
                                       linewidth = 0.70),
      panel.grid.major  = element_line(colour = "#BEBEBE", linewidth = 0.35,
                                       linetype = "dashed"),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = "black", face = "bold",
                                       size = 26),
      axis.title        = element_text(colour = "black", face = "bold",
                                       size = 30),
      axis.title.x      = element_text(margin = margin(t = 10)),
      axis.title.y      = element_text(margin = margin(r = 12)),
      axis.ticks        = element_line(colour = "black", linewidth = 0.50),
      axis.ticks.length = unit(10.0, "pt"),
      legend.position      = c(0.985, 0.065),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = NA, colour = "black",
                                       linewidth = 0.70),
      legend.key        = element_blank(),
      legend.text       = element_text(colour = "black", face = "bold",
                                       size = 19),
      legend.key.width  = unit(0.65, "cm"),
      legend.spacing.y  = unit(0.15, "cm"),
      legend.margin     = margin(0.20, 0.25, 0.20, 0.25, "cm"),
      plot.margin       = margin(0.25, 0.25, 0.25, 0.25, "cm")
    )

  save_plot(p, output_file, width = 2362 / 300, height = 1771 / 300)
}

# Dispatch

if (all(c("eruptions", "waiting") %in% names_lower)) {
  plot_histogram(dat, output_file)
} else if (all(c("row", "column", "method", "tmscore") %in% names_lower)) {
  plot_barplot(dat, output_file)
} else if (all(c("group", "individual", "value") %in% names_lower)) {
  plot_circular(dat, output_file)
} else if (all(c("feature", "auc", "mcc") %in% names_lower)) {
  plot_connected(dat, output_file)
} else {
  stop("Unsupported input data: ", paste(names(dat), collapse = ", "),
       call. = FALSE)
}

cat("Plot saved to:", output_file, "\n")
