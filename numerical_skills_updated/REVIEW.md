# Review of the original numerical-skills material

The original lectures have a useful conceptual route: functions and slopes lead into oscillations, while combinations of vectors lead into matrix transformations and PCA. The gaze-velocity example and the final neural-population figure are particularly helpful connections to neuroscience. The strongest opportunity is to make the intermediate reasoning more explicit while reducing how much students must read on each slide.

The revision keeps those two routes and the original author's attribution. It prioritises mathematical reading over symbolic manipulation. Probability-distribution slides are removed, consistent with the separate statistics module. Covariance remains only as the descriptive bridge needed to explain PCA.

## Main teaching changes

| Recommendation | Why it helps this audience | Implementation |
|:---|:---|:---|
| Begin with questions the mathematics answers | A vocabulary list alone gives little reason to remember unfamiliar terms | Three questions in Part 1; measurement-to-PCA route in Part 2 |
| Explain input, output, parameter and units early | Students can decode an equation before performing algebra | Annotated decay equation; unit checks recur throughout |
| Use one idea per slide and put qualifications in notes | Preserves visual attention and a manageable pace | Short student-facing text, large figures, complete speaker notes |
| Ask for predictions every few concepts | Reveals misconceptions before adding more notation | Decay, velocity units, reciprocal frequency, vector combinations, dimensions, PCA changes |
| Put sampling before the DFT | Makes frequency coordinates and aliasing meaningful | A sample grid and a reproducible aliasing example precede the spectrum |
| Introduce projection before PCA | Explains what the new coordinates actually mean | A dot-product-to-projection sequence; scores distinguished from projected vectors |
| Separate PCA directions, scores and reconstruction | Avoids treating an eigenvector as both an axis and a data point | Five explicit steps with the same simulated data throughout |
| Give a core route and optional extensions | Two short lectures cannot establish fluency in every operation | 28 core slides in Part 1; 30 in Part 2, including titles; extensions after each recap |

All questions have answers in fragments or speaker notes. These are small comprehension checks, not a new assessment or mandatory coding class.

## Part 1: specific corrections and clarifications

| Original location/topic | Finding | Revision |
|:---|:---|:---|
| Functions and introductory notation | Several polynomial plots appear before their input/output role is established | Identify the mapping first; put the three examples together |
| Exponential slide | A stray “Proper” fragment remains in the Rmd | Removed; replace with growth/decay and a time-constant interpretation |
| Logarithm plot | The plotting sequence starts at zero, producing `log(0) = -Inf`; domain restrictions are unstated | Plot strictly positive inputs; state the domain and distinguish log bases |
| Logarithm arithmetic | Joint probabilities take the lecture into the statistics module's territory | Explain multiplicative versus additive change without a probability detour |
| Differentiation derivation | Many nearly identical slides reveal the algebra of one example; the core meaning risks getting lost | Three visual secant-to-tangent panels; compact algebra in an optional slide |
| Numerical differentiation | No explicit denominator/unit check in the applied example | `diff(position)/diff(time)`, interval midpoints, N−1 estimates, and degrees/second |
| Numerical differentiation | Finer mathematical increments and noisy measured data need different cautions | Add the visual example of noise amplification |
| Gaussian/binomial slides and summary | These conflict with the intended exclusion of probability theory; the binomial formula also mixes x and k | Remove from this lecture, rather than expand them |
| Oscillation parameters | Phase is used for a time displacement, which can be confused with phase in radians | Use b, A, f and angular phase phi; explain the equivalent time-shift form in notes |
| Temperature example | A pair of pure sinusoids could be mistaken for a physiological account of a monthly cycle | Label as a synthetic temperature-like teaching signal; no physiological inference |
| Fourier series | “Almost any functions” and unqualified improvement with more terms are too broad | State periodic setting, distinguish series/DFT/FFT, retain the visible Gibbs overshoot |
| Fourier spectra | Amplitude, power and PSD can easily become interchangeable in students' descriptions | Label this as a one-sided amplitude spectrum in degrees C |
| Aliasing | Introduced late with an illustration but no quantitative sampling rule | Show 2 Hz and 8 Hz giving exactly identical 10 Hz samples; explain Nyquist assumptions |
| Long summaries | Dense prose asks students to reread the whole lecture | Three recognition prompts and a short exit question |
| Spatial Fourier example | The photograph's provenance is not recorded alongside the source | Use an entirely R-generated image with broad structure and stripes |

The original video makes aliasing memorable, but it also needs playback support and an explanation of camera exposure/readout. The new core uses an exact sampled-cosine example that prints well. The original video remains untouched and could be played as an optional classroom illustration.

## Fourier translation: changes that affect results

The supplied `fourier_transform.m` is the actual local source for the translation; no dependency on the remote link remains.

1. **Sampling grid.** `linspace(0, 56, 1344)` includes both endpoints, so its spacing is 56/1343 days, rather than exactly 1/24 day. The R grid is `(0:(N-1))/24`: 1344 hourly samples representing a 56-day DFT window, ending at 56−1/24 days.
2. **Frequency units.** All frequencies are cycles/day, because the time vector is in days. Hertz means cycles/second.
3. **Filter mismatch.** The original slide says periods shorter than 2 days are removed, while the active code uses `abs(f) < 1/24`, retaining nonzero periods longer than 24 days. The revision preserves the active code's slow-component example and states its actual cutoff. A 2-day cutoff would be 1/2 cycles/day.
4. **Mean and baseline.** The script explicitly removes the sample mean and offers both a zero-mean filtered change and a reconstruction with the mean restored. DC is named and explained.
5. **Amplitude scaling.** Normalise by N and double only paired positive-frequency bins. DC and, for even N, the Nyquist bin must not be doubled. The condition also handles odd N correctly.
6. **Inverse FFT.** R's inverse FFT needs an explicit division by N; MATLAB `ifft` supplies that scaling. The mask preserves positive and negative frequency partners. These conventions are documented in [R's FFT reference](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html).
7. **Limits of the demonstration.** Complete cycles make peaks easy to recognise. An optional 55-day example shows leakage, and the script explains that a hard cutoff can produce ringing and edge artefacts. This is not a recommended full EEG preprocessing pipeline.

## Part 2: specific corrections and clarifications

| Original location/topic | Finding | Revision |
|:---|:---|:---|
| Vocabulary slide | “Basis” is named but not developed clearly | Define it immediately after span and independence, using the same two vectors |
| First R vector figure | The drawn arrow begins at (−0.5, −0.5) although it is intended as an origin-based coordinate vector | Draw the new arrow from (0,0) |
| 3D vector widget | Requires Plotly/WebGL for a simple dimensionality point; the PDF does not preserve a useful interactive view | Use a labelled 2D diagram and explain the extension to any number of measurements |
| Dot-product drawing (`dot01.png` and its transparent copy) | The handwritten last line gives the squared-length formula as the length: the square root is missing | Define norm as `sqrt(sum(a^2))`; keep the dot-product angle formula with nonzero-vector restriction |
| Dot-product summary | “Gives angular distances” is misleading: a dot product depends on lengths as well as angle | Separate dot product, cosine similarity and angle in notes |
| Matrix product drawings | Hard to edit/project and formula text is embedded in pixels | Recreate the worked matrices as R vector graphics and column-combination equations as LaTeX |
| MATLAB multiplication syntax | `*` / `.*` explanations do not transfer directly to R | Show actual R output for `A * B` and `A %*% B` |
| Dimensions and transpose | Assumed at several crucial steps | Give both a dedicated worked example and repeated shape labels |
| Linear models | Distributional notation introduces a probability assumption, with ambiguous scalar notation for an error vector | Retain an optional numeric design-matrix example without distributional assumptions |
| Network example | Weighted sums risk being presented as the whole network layer | Show weights, bias and activation separately; use a consistent column-vector convention |
| Stretch transformation | Axis scaling can obscure geometric interpretation | Equal-aspect circle/grid before and after the transformation |
| Identity/inverse | Matrix sizes and existence conditions need qualifying; `1/(1+delta)` requires delta≠−1 | State compatible identity sizes and square/full-rank inverse condition in optional material |
| Eigenvectors | “Fixed in direction” misses negative and zero eigenvalues | “Same line of action”; require v≠0 and explain reversal/collapse |
| PCA overview | A large covariance formula and several new concepts arrive together | Introduce centring, covariance, directions, scores and reconstruction separately |
| PCA arrows | Eigenvalue-scaled arrows can be mistaken for lengths in data units | Use arrows of length `2*sqrt(eigenvalue)` and explicitly label them as score SDs |
| PCA interpretation | Directions, scores, scaling choices and signs are not distinguished sufficiently | Define all four and include a misconception check; variance is not biological importance |
| Final paper example | Learners need help reading the plotted axes before interpreting the science | Retain a credited published figure, focus on panel A, and explain what a point and trajectory mean |

The recreated visual content covers vector coordinates, addition, scalar multiplication, span, dot products, projection, matrix multiplication, stretching and eigenvectors. Equations for the Hadamard product's R equivalent, identity, inverse and linear combinations are editable text/code instead of scans. This is a conceptual redraw, not a pixel-by-pixel tracing of every duplicate image.

## PCA translation: what is retained and expanded

I retrieved and read the [MATLAB PCA example linked by the original slides](https://github.com/mattelisi/NeuroMethods/blob/master/examples/PCA/pca_example.m). It simulates 250 observations with `x2 = x1 + noise`, forms `X*X'/250`, finds eigenvectors and draws the first direction. The local `pca.R` instead simulates from a specified covariance using MASS and also illustrates eigenvectors; both informed the revision.

The R tutorial retains the simple related-measurements simulation, adds visible offsets, then subtracts each sample column mean. It uses `t(Xc) %*% Xc / (n-1)`, `eigen()`, and ordinary matrix products to obtain loadings, scores, explained variance and a one-PC reconstruction. It uses no analysis package or high-level PCA routine in the worked steps. R's [`eigen` reference](https://stat.ethz.ch/R-manual/R-devel/library/base/html/eigen.html) specifies column-wise unit eigenvectors, ordering and sign ambiguity.

Calling the uncentred second-moment matrix a sample covariance is only an approximation when the sample means happen to be small. Changing the divisor from n to n−1 rescales eigenvalues but does not change directions or explained-variance fractions. The new implementation makes both choices explicit. A separate validation script compares against [`prcomp`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html), whose SVD implementation provides a useful independent check.

## Whole-folder review and maintenance

The review covers both Rmd sources, their exported HTML/PDF content, the three R scripts and supplied MATLAB file, custom CSS/JavaScript, the hand-drawn figures and image collection, and the two video encodings. `validation/asset_review.csv` records every original file. Generated figure directories, the `bk_part1_files` backup resources and vendored `libs` were assessed as build dependencies/duplicates, not as additional authored lectures or third-party code requiring a security audit.

Several assets are unused by the current Rmd files. These include an unrelated screenshot (`algo_availability.png`), alternative vector sketches, duplicate figure exports, code screenshots and older animation/image variants. They should not be carried into a clean teaching project merely because they are present. None has been deleted from the original directory.

The original `stretch.R` depends on `background_plot` from a prior session; `test_bgroup.R` is a font/bracket experiment; `drawings/white_trasp.sh` uses a machine-specific `~/magick`. The new project avoids all three dependencies. It also replaces xaringan-specific CSS/macros and the misspelled `ration: 16:9` setting with explicit Quarto presentation dimensions. Generated outputs are not the editable source of truth.

The original academic-year attendance/feedback slide contains dated institutional links/QR codes. It has not been transferred into the new academic-year-neutral deck; add the current official slide if it is still required. It is deliberately not regenerated with guessed links.

## Further improvements worth considering

- Test the pace with one or two incoming students: ask them to explain an axis and a matrix product aloud, then use the confusion points to choose where to pause.
- If R is taught elsewhere, coordinate the variable/indexing conventions and give the scripts as follow-up practice. If not, demonstrate selected lines rather than introducing a programming lesson simultaneously.
- For a longer class, add one authentic data example with documented preprocessing. Keep the present synthetic examples as the transparent reference where the underlying components are known.
- Keep a reading checklist beside future paper discussions: units, sampling rate/window, row/column meaning, centring/scaling, and what each axis represents.
- Confirm the actual session length before fixing the optional-slide selection. The current hour-per-part route is a stated provisional assumption.

The remaining uncertainty is instructional pacing with the particular cohort, not the numerical operation of the examples. See `validation/RESULTS.md` for the completed build and validation record.
