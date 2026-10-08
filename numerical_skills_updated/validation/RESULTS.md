# Validation record

Completed 8 October 2026.

## Build and display

- `quarto render` completed for both decks, using Quarto 1.5.57, R 4.5.2, knitr 1.48 and rmarkdown 2.28.
- The pre-render step regenerated all **37 SVG figures** from base R.
- Headless Chrome inspected all **34 Part 1 slides** and **35 Part 2 slides**, including revealed answers, at a 1440×900 browser viewport.
- All **55 Part 1 equations** and **52 Part 2 equations** rendered. No equation error markers, failed images, missing image alternative text, detected slide-boundary overflow, or JavaScript/network errors occurred in the final pass.
- Every content slide has speaker notes: 33 in Part 1 and 34 in Part 2. Title slides have no notes.
- The installed Quarto version's default MathJax configuration mixed APIs. The project uses its supported **KaTeX** renderer to avoid that problem. No separate downloaded equation-renderer bundle is included.
- Browser screenshots were visually reviewed across both decks, with full-size inspection of representative plots, equations, code and the paper figure. This caught and corrected figure-internal label overlap and a misleadingly narrow span illustration that a slide-boundary check alone would miss.
- Both decks were exported through RevealJS's PDF layout, with fragment answers together on the same page and no speaker notes. The final PDFs contain 34 and 35 pages respectively.

The machine-readable browser record is [slide_checks.json](slide_checks.json). These checks establish rendering at the tested viewport and in the generated PDFs; they are not a classroom projection or assistive-technology user test.

## Numerical results

All **25 checks** in [numerical_checks.R](numerical_checks.R) pass, including:

- Exactly hourly spacing for the 1,344-sample Fourier example.
- A full FFT/inverse FFT round trip and a real, correctly centred filtered result.
- Correct one-sided amplitudes at odd and even sample counts, including the highest positive/Nyquist bin.
- Exact recovery of amplitudes 0.2 and 0.3 without noise and exact recovery of the known slow component after filtering.
- Identical sampled values for the 2 Hz and 8 Hz aliasing illustration at a 10 Hz sampling rate.
- Agreement of the explicit sample covariance with `cov(X)`.
- Orthonormal loading vectors satisfying the eigenvector equation, score variances matching eigenvalues, and complete reconstruction from all PCs.
- Agreement with the independent SVD-based `prcomp()` calculation, allowing eigenvector sign changes.
- One-PC reconstruction error equal to the discarded eigenvalue times n−1; restoration of column means; reconstruction invariant to matched sign reversals.

With the fixed seeds, the noisy Fourier example estimates amplitudes **0.2135** (28-day) and **0.3076** (daily), around the intended 0.2 and 0.3. The PCA example explains **86.8%** of sample variance with PC1 and **13.2%** with PC2.

Both R scripts were also run directly from the command line in fresh `Rscript --vanilla` sessions and successfully wrote their demonstration PDFs. Those temporary verification outputs are separate from the two lecture PDFs.

## Original material and references

All **160 original files** match the SHA-256 manifest captured before work began; the original file set is unchanged. This includes the already-untracked `fourier_transform.m`, which remains untracked and untouched. Only `numerical_skills_updated/` was added to the teaching repository.

Both original PDFs were reviewed as page previews and extracted text; both exported HTML slide sources were compared with the Rmd sources. Handwritten/raster illustrations, animation frames and the propeller video were inspected. Every original file is represented in [asset_review.csv](asset_review.csv), including generated/backup files and vendored dependencies; those third-party libraries were classified as build infrastructure rather than separately code-audited.

All relative links and figure paths in the two revised Quarto sources resolve. Reused paper assets are unchanged copies, with attribution in [ASSETS.md](../ASSETS.md). The linked MATLAB PCA script was retrieved and reviewed to inform the R adaptation; the local Fourier MATLAB script was used directly.

The provisional teaching duration is about an hour per part. Actual cohort pacing remains for the instructor to assess; optional material is separated after the recaps.
