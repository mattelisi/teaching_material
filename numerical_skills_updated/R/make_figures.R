# Rebuild every teaching diagram using base R graphics.
# Run from the project directory: Rscript --vanilla R/make_figures.R
# No external data, plotting packages, fonts or internet connection required.

if (!file.exists("R/fourier_transform.R")) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg)) {
    script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
    setwd(dirname(dirname(script_path)))
  }
}
stopifnot(file.exists("R/fourier_transform.R"))
dir.create("figures", showWarnings = FALSE)
fourier <- new.env()
pca <- new.env()
sys.source("R/fourier_transform.R", envir = fourier)
sys.source("R/pca_example.R", envir = pca)

ink <- "#182b3a"
teal <- "#007d8c"
orange <- "#bd571d"
purple <- "#755b9c"
grey <- "#94a3ac"
pale <- "#dce4e5"
paper <- "#faf9f5"

figure <- function(name, draw, width = 10, height = 4.4) {
  svg(file.path("figures", paste0(name, ".svg")), width = width, height = height,
      pointsize = 16, bg = paper, family = "sans")
  on.exit(dev.off())
  par(mar = c(3.5, 3.8, 2.1, 0.8), mgp = c(2.1, 0.6, 0),
      bty = "l", las = 1, col = ink, col.axis = ink, col.lab = ink,
      col.main = ink, fg = ink, cex.main = 1.02)
  draw()
  invisible(name)
}
axes_plane <- function(xlim = c(-3, 4), ylim = c(-3, 4), main = "",
                       xlab = "First coordinate", ylab = "Second coordinate") {
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, xlab = xlab, ylab = ylab, main = main)
  abline(h = 0, v = 0, col = pale)
}
vec <- function(v, colour = teal, from = c(0, 0), lty = 1, lwd = 3) {
  arrows(from[1], from[2], v[1], v[2], col = colour,
         length = 0.13, lwd = lwd, lty = lty)
}

figure("functions", function() {
  par(mfrow = c(1, 3))
  x <- seq(-2, 2, length.out = 300)
  for (j in 1:3) {
    if (j == 3) x <- seq(-6, 3, length.out = 300)
    y <- list(x, x^2, x^3 / 5 + x^2 - 1)[[j]]
    title <- c("A straight line", "A quadratic", "A cubic")[j]
    plot(x, y, type = "l", lwd = 3, col = teal, xlab = "x", ylab = "f(x)", main = title)
    abline(h = 0, v = 0, col = pale)
    lines(x, y, lwd = 3, col = teal)
  }
}, width = 12)
figure("exponential", function() {
  par(mfrow = c(1, 2))
  x <- seq(-2, 2, length.out = 250)
  plot(x, exp(x), type = "l", col = teal, lwd = 3, xlab = "x", ylab = "exp(x)", main = "Exponential growth")
  points(0, 1, pch = 19, col = orange)
  t <- seq(0, 50, length.out = 250)
  plot(t, exp(-t / 10), type = "l", col = teal, lwd = 3,
       xlab = "Time (ms)", ylab = "Fraction remaining", main = "Exponential decay: time constant = 10 ms")
  abline(v = 10, h = exp(-1), col = grey, lty = 2)
  points(10, exp(-1), pch = 19, col = orange)
  text(30, 0.65, "After 10 ms:\nabout 37% remains", col = orange)
})
figure("logarithm", function() {
  x <- seq(0.04, 8, length.out = 500)
  plot(x, log(x), type = "l", lwd = 3, col = teal,
       xlab = "Positive input x", ylab = "ln(x)")
  abline(h = 0, col = pale)
  points(c(1, exp(1)), c(0, 1), pch = 19, col = orange)
  text(c(1, exp(1)), c(0, 1), c("ln(1) = 0", "ln(e) = 1"), pos = 4)
}, width = 6.5)
figure("slope", function() {
  x <- seq(0, 4, length.out = 100)
  plot(x, 2 * x + 1, type = "l", lwd = 3, col = teal,
       xlab = "Time (s)", ylab = "Position (cm)", ylim = c(0, 10))
  segments(1, 3, 3, 3, col = orange, lwd = 3)
  segments(3, 3, 3, 7, col = purple, lwd = 3)
  points(c(1, 3), c(3, 7), pch = 19, cex = 1.2, col = teal)
  text(2, 2, "2 seconds", col = orange)
  text(3.05, 5, "4 cm", pos = 4, col = purple)
}, width = 6.5, height=6)
figure("tangent", function() {
  par(mfrow = c(1, 3))
  x <- seq(-0.1, 3.1, length.out = 300)
  for (h in c(1.5, 0.5, 0.05)) {
    plot(x, x^2, type = "l", lwd = 3, col = teal,
         xlab = "x", ylab = "x squared", main = paste("Gap h =", h), ylim = c(-1, 9))
    slope <- ((1 + h)^2 - 1) / h
    abline(a = 1 - slope, b = slope, col = orange, lwd = 2)
    abline(a = -1, b = 2, col = ink, lty = 2, lwd = 2)
    points(c(1, 1 + h), c(1, (1 + h)^2), pch = 19, col = orange)
    text(0.1, 8.5, paste("Slope =", slope), adj = 0, col = orange, cex = 0.85)
  }
})
figure("derivative_sign", function() {
  x <- seq(-2.5, 2.5, length.out = 300)
  plot(x, x^2, type = "l", lwd = 3, col = teal, xlab = "x", ylab = "f(x)")
  for (a in c(-1.5, 0, 1.5)) {
    z <- a + c(-0.45, 0.45)
    lines(z, a^2 + 2 * a * (z - a), col = orange, lwd = 3)
    points(a, a^2, pch = 19, col = orange)
  }
  text(c(-1.5, 0, 1.5), c(3.7, 1.1, 3.7), c("negative", "zero", "positive"))
}, width = 7)
figure("gaze", function() {
  par(mfrow = c(1, 2))
  t <- seq(0, 0.2, by = 0.002)
  position <- 10 / (1 + exp(-(t - 0.1) / 0.009))
  velocity <- diff(position) / diff(t)
  plot(t, position, type = "b", pch = 16, cex = 0.35, lwd = 2, col = teal,
       xlab = "Time (s)", ylab = "Gaze position (degrees)", main = "A simulated eye movement")
  plot((t[-1] + t[-length(t)]) / 2, velocity, type = "l", lwd = 3, col = orange,
       xlab = "Time (s)", ylab = "Velocity (degrees/s)", main = "Change in position / elapsed time")
})
figure("derivative_noise", function() {
  set.seed(4)
  t <- seq(0, 0.2, by = 0.002)
  x <- 10 / (1 + exp(-(t - 0.1) / 0.009))
  noisy <- x + rnorm(length(x), sd = 0.08)
  par(mfrow = c(1, 2))
  plot(t, noisy, type = "l", col = grey, xlab = "Time (s)", ylab = "Position (degrees)", main = "Small position errors")
  lines(t, x, col = teal, lwd = 3)
  plot(t[-1] - 0.001, diff(noisy) / diff(t), type = "l", col = grey,
       xlab = "Time (s)", ylab = "Velocity (degrees/s)", main = "Larger errors in the derivative")
  lines(t[-1] - 0.001, diff(x) / diff(t), col = teal, lwd = 3)
})
figure("circle", function() {
  par(mfrow = c(1, 2))
  a <- seq(0, 2 * pi, length.out = 300)
  plot(cos(a), sin(a), type = "l", asp = 1, xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3),
       xlab = "cos(angle)", ylab = "sin(angle)", col = grey, main = "One turn = 2 pi radians")
  abline(h = 0, v = 0, col = pale)
  angle <- pi / 3
  vec(c(cos(angle), sin(angle)))
  segments(0, 0, cos(angle), 0, col = orange, lwd = 4)
  segments(cos(angle), 0, cos(angle), sin(angle), col = purple, lwd = 4)
  plot(a, sin(a), type = "l", col = purple, lwd = 3, xaxt = "n",
       xlab = "Angle (radians)", ylab = "Value", main = "Two views of the same turn")
  lines(a, cos(a), col = orange, lwd = 3, lty = 2)
  axis(1, at = c(0, pi, 2 * pi), labels = c("0", "pi", "2 pi"))
  legend("bottomleft", c("sin", "cos"), col = c(purple, orange), lty = c(1, 2), lwd = 3, bty = "n", cex = 0.8)
})
figure("wave_parameters", function() {
  par(mfrow = c(2, 2), mar = c(3.1, 3.5, 1.9, 0.5))
  t <- seq(0, 1, length.out = 500)
  base <- cos(2 * pi * 2 * t)
  alternatives <- list(1 + base, 2 * base, cos(2 * pi * 4 * t), cos(2 * pi * 2 * t + pi / 2))
  titles <- c("Raise the baseline", "Double the amplitude", "Double the frequency", "Shift the phase")
  for (j in 1:4) {
    plot(t, base, type = "l", lty = 2, col = grey, lwd = 2,
         ylim = c(-2.1, 2.1), xlab = "Time (s)", ylab = "Signal", main = titles[j])
    lines(t, alternatives[[j]], col = teal, lwd = 3)
  }
}, height = 5)
figure("sampling", function() {
  t <- seq(0, 1, length.out = 1000)
  ts <- (0:19) / 20
  plot(t, cos(2 * pi * 3 * t), type = "l", col = grey, lwd = 2,
       xlab = "Time (s)", ylab = "Signal", main = "20 evenly spaced observations in one second")
  points(ts, cos(2 * pi * 3 * ts), col = teal, pch = 19, cex = 1.2)
  segments(ts, -1.15, ts, cos(2 * pi * 3 * ts), col = "#007d8c33")
})
figure("aliasing", function() {
  t <- seq(0, 1, length.out = 2000)
  samples <- (0:10) / 10
  plot(t, cos(2 * pi * 8 * t), type = "l", col = orange, lwd = 2,
       xlab = "Time (s)", ylab = "Signal", ylim = c(-1.2, 1.6))
  lines(t, cos(2 * pi * 2 * t), col = teal, lwd = 3, lty = 2)
  points(samples, cos(2 * pi * 2 * samples), pch = 19, cex = 1.2)
  legend("top", c("8 Hz", "2 Hz", "Samples at 10 Hz"),
         col = c(orange, teal, ink), lty = c(1, 2, NA),
         pch = c(NA, NA, 19), lwd = c(2, 3, NA), horiz = TRUE, bty = "n", cex = 0.85)
})
figure("rhythms", function() {
  par(mfrow = c(3, 1), mar = c(3.6, 4.3, 1.6, 0.6))
  t <- fourier$time_days
  values <- list(fourier$slow_component, fourier$daily_component,
                 fourier$temperature - fourier$baseline)
  for (j in 1:3) {
    plot(t, values[[j]], type = "l", col = c(purple, orange, teal)[j],
         xlab = if (j == 3) "Time (days)" else "", ylab = "Change (C)", lwd = 1.5,
         yaxt = "n",
         main = c("Slow rhythm: period 28 days", "Daily rhythm: period 1 day", "Their sum + noise")[j])
    axis(2, at = if (j == 1) c(-0.2, 0, 0.2) else if (j == 2) c(-0.3, 0, 0.3) else c(-0.5, 0, 0.5, 1))
  }
}, height = 5.3)
figure("spectrum", function() {
  par(mfrow = c(1, 2))
  for (j in 1:2) {
    plot(fourier$frequency, fourier$amplitude, type = "h", lwd = 3, col = teal,
         xlim = if (j == 1) c(0, 1.25) else c(0, 0.13), ylim = c(0, 0.38),
         xlab = "Frequency (cycles/day)", ylab = "Amplitude (degrees C)",
         main = c("Two peaks", "Zoom into the slow peak")[j])
    abline(v = if (j == 1) 1 else 1 / 28, col = orange, lty = 2)
    text(if (j == 1) 0.8 else 0.074, 0.3, if (j == 1) "1 cycle/day" else "1/28 cycle/day\n= 28-day period", cex = 0.8)
  }
})
figure("filter", function() {
  par(mfrow = c(1, 2))
  f <- fourier$frequency
  plot(f, fourier$amplitude, type = "h", lwd = 2, col = grey,
       xlim = c(0, 0.16), ylim = c(0, 0.25), xlab = "Frequency (cycles/day)",
       ylab = "Amplitude (degrees C)", main = "Only keep frequencies below 1/24")
  rect(0, 0, fourier$cutoff, 0.25, col = "#007d8c18", border = NA)
  lines(f, fourier$amplitude * (f < fourier$cutoff), type = "h", col = teal, lwd = 3)
  abline(v = fourier$cutoff, col = orange, lty = 2, lwd = 2)
  plot(fourier$time_days, fourier$filtered_change, type = "l", lwd = 3,
       col = teal, xlab = "Time (days)", ylab = "Change (degrees C)", main = "Reconstruct the slow component")
  lines(fourier$time_days, fourier$slow_component, col = orange, lty = 2, lwd = 2)
  legend("topright", c("Filtered", "Known signal"), col = c(teal, orange),
         lty = c(1, 2), lwd = 2, bty = "n", cex = 0.7)
})
figure("fourier_series", function() {
  par(mfrow = c(1, 3))
  x <- seq(-pi, pi, length.out = 1200)
  for (terms in c(1, 3, 15)) {
    y <- rep(0, length(x))
    for (k in seq(1, 2 * terms - 1, by = 2)) y <- y + 4 * sin(k * x) / (pi * k)
    plot(x, y, type = "l", lwd = 3, col = teal, ylim = c(-1.4, 1.4),
         xlab = "Angle (radians)", ylab = "Value", main = paste(terms, if (terms == 1) "term" else "terms"))
    lines(x, sign(sin(x)), col = orange, lty = 2, lwd = 2)
  }
})
figure("phase", function() {
  par(mfrow = c(1, 2))
  t <- (0:199) / 100
  y1 <- cos(2 * pi * 3 * t)
  y2 <- cos(2 * pi * 3 * t + pi / 2)
  plot(t, y1, type = "l", col = teal, lwd = 3, xlim = c(0, 1),
       xlab = "Time (s)", ylab = "Signal", main = "Different timing")
  lines(t, y2, col = orange, lwd = 2, lty = 2)
  f <- (0:100) / 2
  a <- Mod(fft(y1))[1:101] / 200
  a[2:100] <- 2 * a[2:100]
  b <- Mod(fft(y2))[1:101] / 200
  b[2:100] <- 2 * b[2:100]
  plot(f, a, type = "h", col = teal, lwd = 5, xlim = c(0, 8),
       xlab = "Frequency (Hz)", ylab = "Amplitude", main = "The same amplitude spectrum")
  points(f, b, col = orange, pch = 1, cex = 1.2)
})
figure("leakage", function() {
  par(mfrow = c(1, 2))
  for (days in c(56, 55)) {
    N <- days * 24
    t <- (0:(N - 1)) / 24
    y <- cos(2 * pi * t / 28)
    f <- (0:(N / 2)) / days
    a <- Mod(fft(y - mean(y)))[1:length(f)] / N
    a[2:(length(a) - 1)] <- 2 * a[2:(length(a) - 1)]
    plot(f, a, type = "h", col = teal, lwd = 3, xlim = c(0, 0.15), ylim = c(0, 1.05),
         xlab = "Frequency (cycles/day)", ylab = "Amplitude", main = paste(days, "days recorded"))
    abline(v = 1 / 28, col = orange, lty = 2)
  }
})
figure("integral", function() {
  t <- seq(0, 1, length.out = 400)
  v <- 6 * t * (1 - t)
  plot(t, v, type = "n", ylim = c(0, 1.8), xlab = "Time (s)", ylab = "Velocity (cm/s)")
  polygon(c(t, rev(t)), c(v, rep(0, length(t))), col = "#007d8c33", border = NA)
  lines(t, v, col = teal, lwd = 3)
  text(0.5, 0.6, "Area = change in position", col = teal)
})
figure("image_filter", function() {
  n <- 128
  x <- (0:(n - 1)) / n
  blob <- outer(x, x, function(a, b) exp(-((a - 0.5)^2 + (b - 0.5)^2) / 0.045))
  stripes <- outer(x, x, function(a, b) 0.15 * cos(2 * pi * 20 * a))
  input <- blob + stripes
  f <- 0:(n - 1)
  f[f > n / 2] <- f[f > n / 2] - n
  keep <- outer(f, f, function(a, b) sqrt(a^2 + b^2) <= 6)
  low <- Re(fft(fft(input) * keep, inverse = TRUE)) / length(input)
  par(mfrow = c(1, 3), mar = c(0.2, 0.2, 2, 0.2))
  for (j in 1:3) {
    z <- list(input, low, input - low)[[j]]
    image(x, x, z, col = gray.colors(256, start = 0, end = 1), axes = FALSE,
          asp = 1, xlab = "", ylab = "", main = c("Synthetic image", "Low spatial frequencies", "Remaining detail")[j])
  }
})

# LINEAR ALGEBRA -----------------------------------------------------------
figure("data_matrix", function() {
  par(mar = c(0, 0, 0, 0))
  plot.new(); plot.window(xlim = c(0, 8), ylim = c(0, 5))
  values <- matrix(c(4, 8, 2, 6, 9, 4, 3, 6, 1, 7, 10, 5), ncol = 3, byrow = TRUE)
  for (i in 1:4) for (j in 1:3) {
    rect(j + 1, 4 - i, j + 2, 5 - i, border = paper,
         col = if (i == 2) "#c6e2e2" else "#e5ebed")
    text(j + 1.5, 4.5 - i, values[i, j], cex = 1.4)
  }
  text(2.5:4.5, 4.4, c("Neuron 1", "Neuron 2", "Neuron 3"), cex = 0.9)
  text(1.8, 3.5:0.5, paste("Trial", 1:4), adj = 1)
  text(6.5, 2.5, "One row:\none observation", col = teal, cex = 1.1)
  arrows(5.7, 2.5, 5.05, 2.5, col = teal, length = 0.1)
  text(3.5, -0.4, "Toy responses in the same units", cex = 0.9)
}, height = 4)
figure("vector", function() {
  axes_plane(c(-0.3, 4), c(-0.3, 3), xlab = "Measurement 1", ylab = "Measurement 2")
  segments(3, 0, 3, 2, col = grey, lty = 2)
  segments(0, 2, 3, 2, col = grey, lty = 2)
  vec(c(3, 2)); points(3, 2, pch = 19, cex = 1.3, col = teal)
  text(3, 2, "(3, 2)", pos = 3, cex = 1.2)
}, width = 6.5)
figure("vector_operations", function() {
  par(mfrow = c(1, 2))
  axes_plane(c(-0.5, 5.5), c(-0.5, 4.5), main = "Add: head to tail", xlab = "", ylab = "")
  vec(c(1, 2)); vec(c(4, 3), orange, from = c(1, 2)); vec(c(4, 3), purple)
  text(0.7, 1.2, "a", col = teal, pos = 2)
  text(2.7, 2.8, "b", col = orange, pos = 3)
  text(3.3, 1.5, "a + b", col = purple)
  axes_plane(c(-2.8, 4), c(-1.8, 3), main = "Scale: length and possibly direction", xlab = "", ylab = "")
  vec(c(3, 2), orange); vec(c(1.5, 1), teal); vec(c(-1.5, -1), purple)
  text(3, 2, "2a", pos = 3, col = orange)
  text(1.5, 1, "a", pos = 3, col = teal)
  text(-1.5, -1, "-a", pos = 1, col = purple)
})
figure("span", function() {
  par(mfrow = c(1, 2))
  coefficients <- as.matrix(expand.grid(seq(-2, 2, by = 0.2), seq(-2, 2, by = 0.2)))
  for (j in 1:2) {
    A <- if (j == 1) cbind(c(1, 1), c(2, 3)) else cbind(c(1, 1), c(3, 3))
    if (j == 1) {
      # Choose destinations across the plane, then find their coefficients.
      # A small coefficient box would give a narrow parallelogram here.
      destinations <- as.matrix(expand.grid(seq(-6, 6, by = 0.6), seq(-6, 6, by = 0.6)))
      plane_coefficients <- destinations %*% t(solve(A))
      points_xy <- plane_coefficients %*% t(A)
    } else points_xy <- coefficients %*% t(A)
    axes_plane(c(-6, 6), c(-6, 6), main = c("Independent: span a plane", "Dependent: span a line")[j], xlab = "", ylab = "")
    points(points_xy, col = "#007d8c44", pch = 16, cex = 0.4)
    vec(A[, 1], orange); vec(A[, 2], purple)
  }
})
figure("dot_product", function() {
  par(mfrow = c(1, 3))
  for (j in 1:3) {
    theta <- c(pi / 4, pi / 2, 3 * pi / 4)[j]
    axes_plane(c(-1.5, 1.5), c(-0.4, 1.5), xlab = "", ylab = "",
               main = c("Positive", "Zero", "Negative")[j])
    vec(c(1, 0), teal); vec(c(cos(theta), sin(theta)), orange)
    a <- seq(0, theta, length.out = 100)
    lines(0.4 * cos(a), 0.4 * sin(a), col = ink)
    text(0, -0.3, c("acute angle", "right angle", "obtuse angle")[j], cex = 0.8)
  }
}, height = 3.7)
figure("projection", function() {
  axes_plane(c(-0.2, 3.6), c(-0.2, 3.6), xlab = "", ylab = "")
  u <- c(1, 1) / sqrt(2); x <- c(1, 3)
  projected <- sum(x * u) * u
  abline(a = 0, b = 1, col = grey)
  vec(x, orange); vec(u, teal); vec(projected, purple, lwd = 2)
  segments(x[1], x[2], projected[1], projected[2], col = orange, lty = 2, lwd = 2)
  text(0.9, 3.1, "x", col = orange, pos = 2)
  text(0.8, 0.4, "unit direction u", col = teal, pos = 4, cex = 0.8)
  text(2.1, 2.1, "projection", col = purple, pos = 4, cex = 0.8)
}, width = 6.5)
figure("matrix_product", function() {
  par(mar = rep(0.2, 4)); plot.new(); plot.window(xlim = c(0, 12), ylim = c(0, 4))
  draw_matrix <- function(A, left, top, row = NA, column = NA) {
    for (i in 1:nrow(A)) for (j in 1:ncol(A)) {
      fill <- if (isTRUE(i == row)) "#c6e2e2" else if (isTRUE(j == column)) "#f0d8c9" else "#e8edef"
      rect(left + j - 1, top - i, left + j, top - i + 1, col = fill, border = paper)
      text(left + j - 0.5, top - i + 0.5, A[i, j], cex = 1.6)
    }
  }
  A <- matrix(c(1, 1, 2, -1), 2, byrow = TRUE)
  B <- matrix(c(2, 2, 3, 4), 2, byrow = TRUE)
  draw_matrix(A, 0.5, 3.3, row = 1); draw_matrix(B, 4.3, 3.3, column = 2)
  draw_matrix(A %*% B, 8.5, 3.3)
  text(c(3.4, 7.4), 2.3, c("x", "="), cex = 1.5)
  text(c(1.5, 5.3, 9.5), 3.7, c("A", "B", "AB"), font = 2)
  text(6, 0.4, "Row 1 of A  .  column 2 of B:     1 x 2 + 1 x 4 = 6", cex = 1.1)
}, height = 3.6)
figure("network", function() {
  par(mar = rep(0, 4)); plot.new(); plot.window(xlim = c(0, 5), ylim = c(0, 4))
  for (i in 1:3) for (j in 1:2) arrows(1.15, i, 3.8, j + 0.5, col = grey, length = 0.1)
  points(rep(1, 3), 1:3, pch = 21, cex = 6, bg = "#e5f0f0", col = teal)
  points(rep(4, 2), (1:2) + 0.5, pch = 21, cex = 6, bg = "#f4e8df", col = orange)
  text(rep(1, 3), 1:3, c("x3", "x2", "x1")); text(rep(4, 2), (1:2) + 0.5, c("z2", "z1"))
  text(2.5, 3.5, "weights W", col = ink)
  text(1, 0.3, "3 inputs"); text(4, 0.3, "2 weighted sums")
}, width = 6.5, height = 4.2)
figure("transformation", function() {
  par(mfrow = c(1, 2))
  t <- seq(0, 2 * pi, length.out = 300)
  for (j in 1:2) {
    scale_x <- c(1, 2)[j]
    axes_plane(c(-2.5, 2.5), c(-1.6, 1.6), xlab = "", ylab = "", main = c("Input", "Stretch the first coordinate")[j])
    for (k in seq(-1, 1, by = 0.25)) {
      segments(-scale_x, k, scale_x, k, col = pale)
      segments(scale_x * k, -1, scale_x * k, 1, col = pale)
    }
    lines(scale_x * cos(t), sin(t), col = teal, lwd = 3)
    vec(c(scale_x, 0), orange); vec(c(0, 1), purple)
  }
})
figure("eigenvectors", function() {
  par(mfrow = c(1, 3))
  vectors <- list(c(1, 0), c(0, 1), c(1, 1))
  for (j in 1:3) {
    axes_plane(c(-0.3, 2.6), c(-0.3, 1.8), xlab = "", ylab = "",
               main = c("Eigenvalue = 2", "Eigenvalue = 1", "Not an eigenvector")[j])
    v <- vectors[[j]]; transformed <- c(2 * v[1], v[2])
    vec(transformed, orange, lwd = 5); vec(v, teal, lwd = 2)
    if (j == 2) points(0, 1, pch = 1, cex = 1.7, col = teal, lwd = 2)
  }
}, height = 4)
figure("pca_centre", function() {
  par(mfrow = c(1, 2))
  for (j in 1:2) {
    X <- if (j == 1) pca$X else pca$Xc
    plot(X, pch = 16, col = "#007d8c66", asp = 1, xlab = "Measurement 1", ylab = "Measurement 2",
         main = c("Raw observations", "Subtract each column mean")[j])
    m <- colMeans(X)
    abline(v = m[1], h = m[2], col = grey, lty = 2)
    points(m[1], m[2], pch = 4, lwd = 3, cex = 1.5, col = orange)
  }
})
figure("covariance", function() {
  x <- pca$Xc
  plot(x, pch = 16, asp = 1, col = ifelse(x[, 1] * x[, 2] > 0, teal, orange),
       xlab = "Centred measurement 1", ylab = "Centred measurement 2")
  abline(h = 0, v = 0, col = grey)
  legend("topleft", c("Same sign: positive product", "Opposite signs: negative product"),
         col = c(teal, orange), pch = 16, bty = "n", cex = 0.72)
}, width = 6.5)
figure("pca_axes", function() {
  plot(pca$Xc, pch = 16, col = "#007d8c55", asp = 1,
       xlab = "Centred measurement 1", ylab = "Centred measurement 2")
  for (j in 1:2) {
    end <- 2 * sqrt(pca$eigenvalues[j]) * pca$loadings[, j]
    vec(end, c(teal, orange)[j], lwd = 4)
    text(end[1], end[2], c("PC1", "PC2")[j], pos = if (j == 1) 2 else 4, col = c(teal, orange)[j])
  }
}, width = 6.5, height = 5)
figure("pca_scores", function() {
  par(mfrow = c(1, 2))
  for (j in 1:2) {
    z <- if (j == 1) pca$Xc else pca$scores
    plot(z, asp = 1, pch = 16, col = "#007d8c66", xlim = c(-4.5, 4.5), ylim = c(-4.5, 4.5),
         xlab = c("Centred measurement 1", "PC1 score")[j],
         ylab = c("Centred measurement 2", "PC2 score")[j],
         main = c("Original coordinates", "Coordinates along the PC axes")[j])
    abline(h = 0, v = 0, col = pale)
    points(z[1, 1], z[1, 2], pch = 21, bg = orange, cex = 1.8)
  }
})
figure("pca_reconstruction", function() {
  plot(pca$Xc, asp = 1, pch = 16, col = "#94a3ac66",
       xlab = "Centred measurement 1", ylab = "Centred measurement 2")
  segments(pca$Xc[, 1], pca$Xc[, 2], pca$Xc_one_pc[, 1], pca$Xc_one_pc[, 2], col = "#bd571d44")
  points(pca$Xc_one_pc, pch = 16, col = "#007d8c88")
  legend("topleft", c("Original observation", "One-PC reconstruction"),
         pch = 16, col = c(grey, teal), bty = "n", cex = 0.8)
}, width = 6.5, height = 5)
figure("pca_variance", function() {
  v <- 100 * pca$variance_fraction
  x <- barplot(v, names.arg = c("PC1", "PC2"), col = c(teal, orange),
               border = NA, ylim = c(0, 105), ylab = "Variance explained (%)")
  text(x, v + 7, paste0(round(v, 1), "%"), cex = 1.2)
}, width = 6.5)
figure("pca_scaling", function() {
  par(mfrow = c(1, 3))
  for (j in 1:3) {
    X <- pca$Xc
    if (j > 1) X[, 1] <- 10 * X[, 1]
    if (j == 3) X <- scale(X, center = TRUE, scale = TRUE)
    pc <- eigen(cov(X), symmetric = TRUE)
    if (pc$vectors[1, 1] < 0) pc$vectors[, 1] <- -pc$vectors[, 1]
    limits <- max(abs(X)) * c(-1.2, 1.2)
    plot(X, asp = 1, pch = 16, cex = 0.6, col = "#007d8c55", xlim = limits, ylim = limits,
         xlab = "Measurement 1", ylab = "Measurement 2", main = c("Original units", "First variable x 10", "Then standardise")[j])
    vec(2 * sqrt(pc$values[1]) * pc$vectors[, 1], orange)
  }
})
cat("Rebuilt", length(list.files("figures", pattern = "[.]svg$")), "R-generated SVG figures.\n")
