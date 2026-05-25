# lpari

`lpari` implements **LPA-RI**, a recovery-calibrated index for choosing the
number of profiles in small-sample latent profile analysis.

The package includes:

- a lightweight diagonal Gaussian latent profile fitter;
- AIC, BIC, CAIC, SABIC, ICL, entropy, and class-size summaries;
- a conservative null gate for deciding whether a one-profile structure should
  be retained before interpreting LPA-RI;
- LPA-RI scoring with bundled Study 1 coefficients;
- calibrated plausibility over candidate values of `K`;
- worked examples for Old Faithful, Reaven-Miller diabetes, `iris`, and
  Holzinger-Swineford.

LPA-RI is experimental research software. It is intended to complement, not
replace, substantive theory, graphical diagnostics, robustness checks, and
external validation.

## Installation

From a local clone:

```r
install.packages(".", repos = NULL, type = "source")
library(lpari)
```

From GitHub, after the repository is published:

```r
install.packages("remotes")
remotes::install_github("YOUR-USER/lpari")
library(lpari)
```

To unload it from an R session:

```r
detach("package:lpari", unload = TRUE)
```

## Quick start

```r
library(lpari)

ex <- load_lpari_example("old_faithful")

fit <- fit_lpari(
  ex$data[, ex$indicators],
  k = 1:5,
  n_starts = 50,
  id = ex$data[[ex$id]],
  seed = 20260525
)

fit
fit$null_gate
fit$selection
fit$posterior

plot_lpari_posterior(fit)
plot_lpari_profiles(fit)
```

## Bundled examples

```r
available_lpari_examples()

faithful_fit <- lpari_example("old_faithful", n_starts = 25)
diabetes_fit <- lpari_example("reaven_miller_diabetes", n_starts = 25)
iris_fit <- lpari_example("iris", n_starts = 25)
hs_fit <- lpari_example("holzinger_swineford", n_starts = 25)
```

## Interpretation

`posterior_k` is a temperature-calibrated plausibility distribution over the
candidate profile counts that were fitted. It should be read as calibrated
enumeration uncertainty, not as a generative Bayesian posterior.

LPA-RI should not be used as a stand-alone test that latent profiles exist.
Before interpreting the LPA-RI-selected profile count, inspect `fit$null_gate`
or call `lpari_null_gate()` on a candidate table. If the gate recommends
`retain_K1`, report the one-profile solution and treat any multiclass output as
exploratory.

The first release uses the Study 1 coefficients trained in the manuscript
simulations. The strongest use case is small-sample enumeration under difficult
conditions where classical information criteria often overfit.

The Reaven-Miller diabetes data provide a small applied clinical example with
external diagnostic labels. Iris and Holzinger-Swineford are included as
cautionary supplementary examples: their external labels should not be
interpreted as guaranteed latent-profile ground truth.
