# Numerical skills for neuroscience — revised teaching material

Two introductory lectures for MSc neuroscience students coming from psychology. The aim is to interpret mathematical notation and figures in papers, with minimal assumed quantitative background.

The original `../numerical_skills/` directory is preserved. This directory is a separate, editable Quarto project, not an in-place conversion.

## Start here

- [Part 1 slides](part1.html) — functions, derivatives, sampling and Fourier analysis. [PDF](part1.pdf) · [Editable source](part1.qmd).
- [Part 2 slides](part2.html) — vectors, matrices, projection and PCA. [PDF](part2.pdf) · [Editable source](part2.qmd).
- [Review and suggested improvements](REVIEW.md) — what changed, why, and what could be developed further.
- [Fourier example in R](R/fourier_transform.R) and [PCA example in R](R/pca_example.R) — complete, commented scripts with prediction exercises.
- [Figure generator](R/make_figures.R) — recreates all 37 mathematical diagrams using base R graphics.
- [Asset review](validation/asset_review.csv) — inventory and disposition of every original file.

Open either HTML file in a browser. Arrow keys navigate; `Esc` shows the slide overview; `S` opens speaker notes (a local server can help if the browser restricts this). Notes contain explanations, pacing and exercise answers. Fragment answers appear on the next advance. The mathematical renderer may need an internet connection, as agreed; external references also require one. Embedded figures, styles and slide libraries travel with the HTML. Links to the R scripts require the accompanying `R/` directory.

## Teaching route

There are 34 slides in Part 1 (28 core including the title, then 6 optional) and 35 in Part 2 (30 core including the title, then 5 optional). A provisional hour per part is assumed; adjust to the class's pace. Stop at the recap before entering the optional slides.

| Approximate time | Part 1 | Part 2 |
|:---|:---|:---|
| 0–12 minutes | Functions, notation, exponential and log | Data, vectors, combinations and span |
| 12–25 minutes | Slope, derivative, sampled gaze and units | Dot products, projection, transpose and multiplication |
| 25–38 minutes | Oscillations, frequency, sampling and aliasing | Transformations, eigenvectors and centring |
| 38–54 minutes | Fourier views, spectrum and filtering | Covariance, PCA directions, scores and reconstruction |
| 54–60 minutes | Methods sentence, exit check, questions | PCA check, paper figure, exit check, questions |

This is an instructor-led lecture, not a compulsory R practical. Explain the code's sequence of ideas; students need not type along. For 45 minutes, make the R implementation slides follow-up reading and shorten the algebra demonstrations. For 90 minutes, run the scripts live and let pairs try one modification at a time. The optional slides retain useful material such as differentiation rules, image filtering, inverse matrices, linear models and network layers.

For students who are unfamiliar with covariance, use the sign-of-products picture slowly. It supplies only what the PCA explanation needs; the course still does not introduce probability distributions or inference.

## Edit and render

Required for rebuilding: Quarto, R, and the R packages `knitr` and `rmarkdown`. The numerical examples and figure generator use only packages supplied with R. No tidyverse, MASS, plotting toolkit or machine-learning package is required. The project has been built with Quarto 1.5.57, R 4.5.2, knitr 1.48 and rmarkdown 2.28.

From this directory:

```sh
quarto render
```

The pre-render step runs `R/make_figures.R`, so changing a plot or simulation updates the SVGs automatically. For one deck:

```sh
quarto render part1.qmd
quarto preview part2.qmd
```

Edit wording, equations and speaker notes in the `.qmd` files, figure code in `R/make_figures.R`, and colours/type in `theme.scss`. The figure generator sources both worked examples, so their calculations and the plotted results stay consistent. Render from a clean session; the scripts do not depend on objects from the original lectures.

The HTML decks can also be printed using the browser's PDF layout (`?print-pdf` appended to the URL). Print to landscape with backgrounds enabled and browser headers/footers disabled; choose “Save as PDF”. Any supplied PDFs are static exports: reveal fragments are shown together and speaker notes are not printed.

## Run the examples

Interactively in R, from this directory:

```r
source("R/fourier_transform.R")
plot_fourier_example()

source("R/pca_example.R")
plot_pca_example()
```

Source one example at a time when teaching: each deliberately exposes its variables in the workspace. For independent command-line runs that save plots:

```sh
Rscript --vanilla R/fourier_transform.R fourier_example.pdf
Rscript --vanilla R/pca_example.R pca_example.pdf
```

Each command accepts an optional output PDF filename and writes only to that location. Without an argument it uses the filename shown above in the current working directory. Running with `source()` calculates variables and defines the plotting function without writing a PDF.

## Validation and attribution

```sh
Rscript --vanilla validation/numerical_checks.R
```

The checks cover known Fourier amplitudes, odd/even sample counts, Nyquist scaling, inverse reconstruction, the actual aliasing example, covariance, orthogonal PCA directions, score variances and reconstruction. They compare the explicit PCA calculation against base R's SVD-based `prcomp()` as an independent reference; `prcomp()` is not part of the worked teaching script.

All new diagrams are generated from the included R code. The paper figure is credited in the slides and [ASSETS.md](ASSETS.md). [Original file hashes](validation/original_sha256.json) record the untouched input tree, including the user-supplied untracked MATLAB file. Build and inspection results are recorded in [validation/RESULTS.md](validation/RESULTS.md).

Quarto's [RevealJS guide](https://quarto.org/docs/presentations/revealjs/) documents the presentation format and speaker notes. R's official documentation describes the conventions for [fft](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html), [eigen](https://stat.ethz.ch/R-manual/R-devel/library/base/html/eigen.html) and [prcomp](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html).
