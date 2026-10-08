# PCA STEP BY STEP: describe two measurements using a new pair of axes
# Based on Matteo Lisi's MATLAB teaching example:
# https://github.com/mattelisi/NeuroMethods/blob/master/examples/PCA/pca_example.m
# This version explicitly centres the data and uses sample covariance (n - 1).
# Base R only. No machine-learning packages and no prcomp() in the worked steps.
# Source this file; call plot_pca_example() to plot.
# Command line: Rscript --vanilla R/pca_example.R pca_example.pdf

# 1. Simulate two related measurements -------------------------------------
set.seed(2)
n <- 250
x1 <- rnorm(n)
x2 <- x1 + rnorm(n)                  # preserve the original example's relation
X <- cbind(measurement_1 = x1 + 5, measurement_2 = x2 + 10)
# Rows = observations (e.g. trials); columns = measured variables.
# The offsets make centring visible. These are toy measurements, not real data.

# 2. Centre each column: subtract that variable's average -------------------
column_means <- colMeans(X)
Xc <- X
for (j in 1:ncol(X)) {
  Xc[, j] <- X[, j] - column_means[j]
}
# colMeans(Xc) is now approximately c(0, 0). Do not divide by each column's SD:
# both measurements have the same units, and we retain their original scale.

# 3. Compute the covariance matrix -----------------------------------------
S <- t(Xc) %*% Xc / (n - 1)
# t() swaps rows and columns; %*% is matrix multiplication.
# Each entry averages products of centred values.
# On the diagonal: each variable's variance. Off diagonal: their covariance.
# This gives the same result as cov(X), which centres internally.

# 4. Find the new directions ------------------------------------------------
decomposition <- eigen(S, symmetric = TRUE)
eigenvalues <- decomposition$values  # sorted from largest to smallest by R
loadings <- decomposition$vectors   # unit directions, one per column
colnames(loadings) <- c("PC1", "PC2")
rownames(loadings) <- colnames(X)
# S %*% loadings[, 1] = eigenvalues[1] * loadings[, 1]
# PC1 maximises the sample variance of projections onto a UNIT direction.
# PC2 captures the remaining variation along a perpendicular direction.

# Choose consistent signs for figures. v and -v define the SAME PCA axis.
# If we reverse a direction, its scores reverse too; the reconstruction agrees.
for (j in 1:ncol(loadings)) {
  if (loadings[1, j] < 0) loadings[, j] <- -loadings[, j]
}

# 5. Project observations onto those directions ----------------------------
scores <- Xc %*% loadings
# scores[i, 1] is the dot product of row i and the first loading vector.
# 'Loadings' means directions/weights here; 'scores' are the new coordinates.
# Papers sometimes use different loading conventions: always check the methods.
variance_fraction <- eigenvalues / sum(eigenvalues)

# 6. Keep one coordinate, then reconstruct two approximate measurements -----
scores_pc1 <- scores[, 1, drop = FALSE]   # n x 1 matrix, not a bare vector
direction_pc1 <- loadings[, 1, drop = FALSE]  # 2 x 1 matrix
Xc_one_pc <- scores_pc1 %*% t(direction_pc1)
X_one_pc <- Xc_one_pc
for (j in 1:ncol(X)) {
  X_one_pc[, j] <- Xc_one_pc[, j] + column_means[j]
}
# Keeping both PCs reconstructs the centred data: scores %*% t(loadings).
# The one-PC approximation lies on a line; omitted PC2 variation is lost.

plot_pca_example <- function() {
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  par(mfrow = c(2, 2), mar = c(4, 4, 2.6, 1), las = 1, bty = "l")
  plot(X, asp = 1, pch = 16, col = "#007d8c66", main = "1. Raw measurements")
  points(column_means[1], column_means[2], pch = 4, cex = 2, lwd = 3)
  plot(Xc, asp = 1, pch = 16, col = "#007d8c55", main = "2. Centre, then find directions")
  abline(h = 0, v = 0, col = "grey80")
  colours <- c("#007d8c", "#bd571d")
  for (j in 1:2) {
    # Arrow length = 2 standard deviations of scores (NOT 2 variances).
    end <- 2 * sqrt(eigenvalues[j]) * loadings[, j]
    arrows(0, 0, end[1], end[2], col = colours[j], lwd = 3, length = 0.12)
  }
  plot(scores, asp = 1, pch = 16, col = "#007d8c66",
       xlab = "PC1 score", ylab = "PC2 score", main = "3. New coordinates")
  abline(h = 0, v = 0, col = "grey80")
  plot(Xc, asp = 1, pch = 16, col = "grey75", main = "4. Keep PC1; discard PC2")
  segments(Xc[, 1], Xc[, 2], Xc_one_pc[, 1], Xc_one_pc[, 2], col = "#bd571d44")
  points(Xc_one_pc, pch = 16, col = "#007d8c66")
}

# Try it:
# A. Add 100 to measurement_1. After centring, do the PCA directions change?
# B. Multiply measurement_1 by 100 instead. Why do the directions change?
# C. Reverse loadings[, 1], then recompute steps 5 and 6. What changes?
# D. Set x2 <- rnorm(n). Is a one-dimensional summary still as useful?
# Extension: standardise using scale(X, center = TRUE, scale = TRUE), then
# repeat steps 3--6. Standardising changes the question by giving each variable
# equal variance. A constant column cannot be standardised this way.
# Large variance does not establish biological importance or causation.

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output <- if (length(args)) args[1] else "pca_example.pdf"
  pdf(output, width = 10, height = 9)
  plot_pca_example()
  dev.off()
  print(round(100 * variance_fraction, 1))
  cat("Percent variance explained by PC1 and PC2. Saved", output, "\n")
}
