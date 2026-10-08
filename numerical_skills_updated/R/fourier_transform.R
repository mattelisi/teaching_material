# FOURIER TRANSFORM: two rhythms hidden in one sampled signal
# An R adaptation of numerical_skills/fourier_transform.m by Matteo Lisi.
# Uses only base R and packages included with R. All data are simulated.
# Read and run sections 1--6 in order. Then try the questions at the end.
# Source this file to calculate the example; call plot_fourier_example() to plot.
# Command line: Rscript --vanilla R/fourier_transform.R fourier_example.pdf

# 1. Make an exactly hourly time grid --------------------------------------
set.seed(10)
samples_per_day <- 24
duration_days <- 56
N <- samples_per_day * duration_days
time_days <- (0:(N - 1)) / samples_per_day
# Last observation: 56 - 1/24 days. Do not include both ends of the interval:
# seq(0, 56, length.out = N) would have a spacing different from 1/24 day.

# 2. Add two rhythms and measurement noise --------------------------------
baseline <- 36.8
slow_component <- 0.2 * cos(2 * pi * (time_days - 16) / 28)
daily_component <- 0.3 * cos(2 * pi * (time_days - 14 / 24))
temperature <- baseline + slow_component + daily_component + rnorm(N, sd = 0.2)
# This deliberately simple temperature-like signal is NOT a physiological
# model of a menstrual cycle or a real recording. Time is in DAYS throughout.

# 3. Separate the mean (the zero-frequency or DC component) ----------------
temperature_mean <- mean(temperature)
centred_temperature <- temperature - temperature_mean
Y <- fft(centred_temperature)
# Y contains complex coefficients: together they encode amplitude AND phase.
# Mod() below keeps the magnitude; it is enough to locate frequency peaks.

# 4. Build a one-sided AMPLITUDE spectrum ----------------------------------
positive_bins <- 0:floor(N / 2)
frequency <- positive_bins * samples_per_day / N  # cycles per day, not Hz
amplitude <- Mod(Y[positive_bins + 1]) / N        # R indexing starts at 1

# A real signal has matching positive/negative frequency magnitudes. Combine
# each pair by doubling. DC has no pair; the Nyquist bin (even N) has no pair.
paired_bins <- positive_bins > 0 & positive_bins < N / 2
amplitude[paired_bins] <- 2 * amplitude[paired_bins]
# The '< N/2' rule also works for odd N: its last positive bin HAS a partner.
# This is amplitude in degrees C; it is not power or power spectral density.

# 5. Keep only the slowest Fourier components ------------------------------
# Use the native FFT order: 0, positive frequencies, then negative frequencies.
signed_frequency <- (0:(N - 1)) * samples_per_day / N
signed_frequency[signed_frequency > samples_per_day / 2] <-
  signed_frequency[signed_frequency > samples_per_day / 2] - samples_per_day

cutoff <- 1 / 24                               # cycles per DAY
keep <- abs(signed_frequency) < cutoff         # periods LONGER than 24 days
Y_filtered <- Y
Y_filtered[!keep] <- 0
# Treat BOTH positive and negative partners equally, so the result stays real.
# The original slide's 'shorter than 2 days' would require a cutoff of 1/2,
# not 1/24. Here we follow the supplied MATLAB code's cutoff, explicitly.

# 6. Return to the time domain ---------------------------------------------
filtered_complex <- fft(Y_filtered, inverse = TRUE) / N
filtered_change <- Re(filtered_complex)
filtered_temperature <- temperature_mean + filtered_change
# Unlike MATLAB ifft(), R's inverse fft() needs an explicit division by N.
# Re() discards tiny numerical imaginary parts, not a genuine complex signal.
# filtered_change has zero mean; filtered_temperature restores the baseline.

peak_table <- data.frame(
  rhythm = c("28-day", "daily"),
  frequency_cycles_per_day = c(1 / 28, 1),
  expected_amplitude = c(0.2, 0.3),
  estimated_amplitude = amplitude[c(which.min(abs(frequency - 1 / 28)),
                                    which.min(abs(frequency - 1)))]
)
# With 56 days, frequency bins are 1/56 cycles/day apart. These two signals
# fall exactly on bins 2 and 56 (R indices 3 and 57). Noise changes estimates.

# Plotting is kept separate so the slides can reuse the same calculations.
plot_fourier_example <- function() {
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  par(mfrow = c(3, 1), mar = c(4, 4.5, 2.1, 1), las = 1, bty = "l")
  plot(time_days, temperature, type = "l", col = "#007d8c",
       xlab = "Time (days)", ylab = "Temperature (degrees C)",
       main = "Simulated recording: two rhythms + noise")
  plot(frequency, amplitude, type = "h", xlim = c(0, 1.3), col = "#007d8c",
       xlab = "Frequency (cycles/day)", ylab = "Amplitude (degrees C)",
       main = "The 28-day and daily rhythms give two peaks")
  points(peak_table$frequency_cycles_per_day,
         peak_table$estimated_amplitude, pch = 19, col = "#bd571d")
  plot(time_days, filtered_change, type = "l", lwd = 2, col = "#007d8c",
       xlab = "Time (days)", ylab = "Change (degrees C)",
       main = "Keep periods longer than 24 days; mean removed")
  lines(time_days, slow_component, col = "#bd571d", lty = 2, lwd = 2)
  legend("topright", c("Filtered estimate", "Known slow component"),
         col = c("#007d8c", "#bd571d"), lty = c(1, 2), bty = "n")
}

# Try it: change ONE quantity, rerun the file, and predict before plotting.
# A. Double daily_component's amplitude. Which peak changes?
# B. Change duration_days to 55. Why are the peaks less concentrated?
#    The table now reports the closest bins, which need not capture all energy.
# C. Set cutoff to 1/2. Which rhythms survive? How much noise remains?
# D. Replace 'keep' with abs(abs(signed_frequency) - 1) < 0.05.
#    You now have a band-pass filter: which rhythm should return?
# A hard spectral cutoff is for demonstration: finite records are treated as
# periodic and can show edge effects/ringing. Practical EEG analysis needs
# appropriate filtering, windows, artefact handling and uncertainty estimates.

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output <- if (length(args)) args[1] else "fourier_example.pdf"
  pdf(output, width = 10, height = 10)
  plot_fourier_example()
  dev.off()
  print(peak_table, row.names = FALSE)
  cat("Saved", output, "\n")
}
