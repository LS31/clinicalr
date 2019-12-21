# clinicalr: Clinical calculations for R

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/clinicalr)](https://CRAN.R-project.org/package=clinicalr)
<!-- badges: end -->
  
This is **clinicalr**: a collection of R functions to perform (basic) clinical calculations in medical research. Pull requests are more than welcome.

# Functions at this time

- Anthropometry
  - Body mass index (BMI, Quetelet index).
  - Body surface area (BSA), according to Monsteller or Du Bois.
- Cardiology
  - Corrected QT interval (QTc) from a electrocardiogram, according to Bazett or Fridericia.
- Metabolism  
  - Presence of the metabolic syndrome, according to the [updated NCEP ATPIII criteria (Grundy, 2005)](http://www.ncbi.nlm.nih.gov/pubmed/16157765).

# Design philosophy

Practical input units
: Units used in the functions can be inconsistent (m versus cm, s versus ms) on purpose, to best suit the most common usage of certain formulae.

Explicit output units
: By relying on the _units_ package, any output of a function will have a explicit unit. If you do not want this, run the output through `drop_units(x)` from the _units_ package to convert to numerics again.

Single entry functions
: If multiple methods or formulae are known for the same phenomenon (e.g. body surface are), a single function is provided and the specific method can be specified as an argument. As a consequence, the package is slightly opinionated: we specify a default method, if possible. This approach is not implemented if the parameters for functions vary to much.

Fail-fast
: If certain conditions are not explicitly provided, err on the side of caution (e.g. a fasting state has to be provided in order for a glucose to be considered a fasting glucose). 

References and caveats
: Every function will have references section and a section on caveats. (However, despite our best efforts of quality control, the functions are provided as-is, without any guarantees, and users would be wise to check the output for quality and accuracy.)

Please note that the 'clinicalr' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

# How to install

**clinicalr** is not (yet) available on CRAN. Install devtools. Then use devtools to install **clinicalr** directly from GitHub.

```{r}
install.packages("devtools")
devtools::install_github("ls31/clinicalcalculations")
```
# How to update

```{r}
devtools::install_github("lc31/clinicalcalculations")
```