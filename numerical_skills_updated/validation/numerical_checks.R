# Meaningful checks of the worked numerical examples, independent of Quarto.
# Run from numerical_skills_updated: Rscript --vanilla validation/numerical_checks.R
# prcomp() is used only here as an independent reference, not in the tutorial.
stopifnot(file.exists("R/fourier_transform.R"))
check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  cat("PASS:", message, "\n")
}
near <- function(a, b, tolerance = 1e-10) max(abs(a - b)) < tolerance

fourier <- new.env()
sys.source("R/fourier_transform.R", envir = fourier)
with(fourier, {
  check(length(temperature) == 1344, "Fourier sample count")
  check(near(diff(time_days), 1 / samples_per_day), "Exactly hourly time grid")
  check(near(fft(Y, inverse = TRUE) / N, centred_temperature), "Full FFT round trip")
  check(max(abs(Im(filtered_complex))) < 1e-12, "Symmetric mask yields a real signal")
  check(abs(mean(filtered_change)) < 1e-12, "Filtered change is centred")
  check(abs(mean(filtered_temperature) - temperature_mean) < 1e-12,
        "Reconstruction restores baseline")
  check(max(abs(peak_table$estimated_amplitude - c(0.2, 0.3))) < 0.03,
        "Noisy peak amplitudes are close to simulated amplitudes")
})

# Evaluate the actual teaching script with known input signals. Override only
# the sample count and simulated temperature; all FFT/scaling/filter code runs.
script <- parse("R/fourier_transform.R")
run_known_signal <- function(size, signal) {
  env <- new.env()
  for (statement in script) {
    assigned <- if (is.call(statement) && identical(statement[[1]], as.name("<-")))
      as.character(statement[[2]]) else ""
    if (identical(assigned, "N")) env$N <- size
    else if (identical(assigned, "temperature")) env$temperature <- signal
    else eval(statement, envir = env)
  }
  env
}
for (size in c(127, 128)) {
  i <- 0:(size - 1)
  highest <- floor(size / 2)
  x <- 5 + 1.2 * cos(2 * pi * 3 * i / size + 0.7) +
    0.4 * cos(2 * pi * highest * i / size)
  result <- run_known_signal(size, x)
  check(near(result$amplitude[c(4, highest + 1)], c(1.2, 0.4)),
        paste("One-sided amplitudes, including last bin, for N =", size))
  check(abs(result$amplitude[1]) < 1e-12,
        paste("Mean removal / DC bin for N =", size))
}
size <- 1344
t <- (0:(size - 1)) / 24
slow <- 0.2 * cos(2 * pi * (t - 16) / 28)
x <- 36.8 + slow + 0.3 * cos(2 * pi * (t - 14 / 24))
clean <- run_known_signal(size, x)
check(near(clean$peak_table$estimated_amplitude, c(0.2, 0.3)), "Exact noiseless peak amplitudes")
check(near(clean$filtered_change, slow), "Low-pass reconstruction of known slow component")
check(near(cos(2 * pi * 8 * (0:10) / 10), cos(2 * pi * 2 * (0:10) / 10)),
      "Aliasing illustration has identical samples")

pca <- new.env()
sys.source("R/pca_example.R", envir = pca)
with(pca, {
  check(near(colMeans(Xc), c(0, 0)), "PCA column centring")
  check(near(S, cov(X)), "Explicit covariance agrees with cov()")
  check(near(t(loadings) %*% loadings, diag(2)), "Unit orthogonal loading vectors")
  check(near(S %*% loadings, loadings %*% diag(eigenvalues)), "Eigenvector equation")
  check(near(cov(scores), diag(eigenvalues)), "Score variances equal eigenvalues")
  check(near(scores %*% t(loadings), Xc), "All PCs reconstruct centred data")
  check(near(colMeans(X_one_pc), colMeans(X)), "One-PC reconstruction restores means")
  check(near(sum((Xc - Xc_one_pc)^2), (n - 1) * eigenvalues[2]),
        "Discarded eigenvalue equals residual sum of squares / (n - 1)")
  reference <- prcomp(X, center = TRUE, scale. = FALSE)
  check(near(reference$sdev^2, eigenvalues), "Eigenvalues agree with independent SVD-based prcomp()")
  check(near(abs(t(loadings) %*% reference$rotation), diag(2)),
        "Principal directions agree with prcomp(), allowing sign changes")
  alternative <- scores
  alternative[, 1] <- -alternative[, 1]
  directions <- loadings
  directions[, 1] <- -directions[, 1]
  check(near(alternative %*% t(directions), Xc), "Sign changes leave reconstruction unchanged")
})
cat("All numerical checks passed.\n")
