args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop(
    "Usage: Rscript Plot2311067.R <input.txt> <output.jpg>",
    call. = FALSE
  )
}

input_file <- args[1]
output_file <- args[2]

if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file, call. = FALSE)
}

# Read data 
dat <- read.table(
  input_file,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

dat$Xtmscore <- as.numeric(dat$Xtmscore)
dat$Ytmscore <- as.numeric(dat$Ytmscore)

# Classify
type_lower <- tolower(trimws(dat$type))

is_circle <- type_lower %in% c("easy", "triv")
is_cross  <- type_lower %in% c("hard", "very")

jpeg(
  filename = output_file,
  width = 776,
  height = 699,
  units = "px",
  res = 120,
  quality = 100
)

tryCatch({

  # Graphics parameters
  par(
    family = "sans",
    font = 2,
    mar = c(5.6, 6.0, 0.7, 0.7),
    mgp = c(3.0, 0.8, 0),
    tcl = -0.22,
    las = 1,
    xaxs = "i",
    yaxs = "i",
    pty = "s",
    lwd = 1.2
  )

  # Initialize an empty square plot
  plot(
    NA,
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i",
    asp = 1
  )

  # Red crosses: hard and very
  points(
    dat$Xtmscore[is_cross],
    dat$Ytmscore[is_cross],
    pch = 4,
    col = grDevices::adjustcolor(
      "#C94F48",
      alpha.f = 0.72
    ),
    cex = 1.05,
    lwd = 1.8
  )

  # Blue hollow circles: easy and triv
  points(
    dat$Xtmscore[is_circle],
    dat$Ytmscore[is_circle],
    pch = 1,
    col = grDevices::adjustcolor(
      "#5276A8",
      alpha.f = 0.78
    ),
    cex = 0.95,
    lwd = 1.8
  )

  # Diagonal reference line: y = x
  abline(
    a = 0,
    b = 1,
    col = "black",
    lwd = 1.7
  )

  # Dashed threshold lines at 0.5
  abline(
    v = 0.5,
    col = "black",
    lwd = 1.5,
    lty = 2
  )

  abline(
    h = 0.5,
    col = "black",
    lwd = 1.5,
    lty = 2
  )

  # Major ticks every 0.2
  major_ticks <- seq(0, 1, by = 0.2)

  # Minor ticks every 0.05, excluding major ticks
  all_ticks <- seq(0, 1, by = 0.05)

  minor_ticks <- all_ticks[
    !vapply(
      all_ticks,
      function(z) {
        any(abs(z - major_ticks) < 1e-8)
      },
      logical(1)
    )
  ]

  # Bottom axis
  axis(
    side = 1,
    at = major_ticks,
    labels = format(major_ticks, trim = TRUE),
    lwd = 1.2,
    lwd.ticks = 1.2,
    cex.axis = 1.15,
    font = 2
  )

  # Left axis
  axis(
    side = 2,
    at = major_ticks,
    labels = format(major_ticks, trim = TRUE),
    lwd = 1.2,
    lwd.ticks = 1.2,
    cex.axis = 1.15,
    font = 2
  )

  # Top and right axes, without labels
  axis(
    side = 3,
    at = major_ticks,
    labels = FALSE,
    lwd = 1.2,
    lwd.ticks = 1.2
  )

  axis(
    side = 4,
    at = major_ticks,
    labels = FALSE,
    lwd = 1.2,
    lwd.ticks = 1.2
  )

  # Minor ticks on all four sides
  axis(
    side = 1,
    at = minor_ticks,
    labels = FALSE,
    tcl = -0.10,
    lwd = 0,
    lwd.ticks = 1.0
  )

  axis(
    side = 2,
    at = minor_ticks,
    labels = FALSE,
    tcl = -0.10,
    lwd = 0,
    lwd.ticks = 1.0
  )

  axis(
    side = 3,
    at = minor_ticks,
    labels = FALSE,
    tcl = -0.10,
    lwd = 0,
    lwd.ticks = 1.0
  )

  axis(
    side = 4,
    at = minor_ticks,
    labels = FALSE,
    tcl = -0.10,
    lwd = 0,
    lwd.ticks = 1.0
  )

  # Axis titles
  mtext(
    "TM-score of I-TASSER",
    side = 1,
    line = 3.15,
    cex = 1.45,
    font = 2
  )

  mtext(
    "TM-score of C-I-TASSER",
    side = 2,
    line = 3.65,
    cex = 1.45,
    font = 2,
    las = 0
  )

  # Draw the plot border
  box(lwd = 1.2)

}, finally = {
  dev.off()
})

cat("Plot saved to:", output_file, "\n")
