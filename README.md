# clinicalr: Clinical calculations for R

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/clinicalr)](https://CRAN.R-project.org/package=clinicalr)
<!-- badges: end -->

The clinicalr package is a collection of R functions to perform (basic) clinical calculations in medical research. **[User documentation can be found on GitHub.io](https://ls31.github.io/clinicalr/).** 

Help and pull requests are more than welcome.

# Functions

- *Anthropometry*
  - Body mass index (BMI, Quetelet index).
  - Body surface area (BSA).
- *Cardiology*
  - Corrected QT interval (QTc) from a electrocardiogram.
- *Metabolism*  
  - Presence of the metabolic syndrome.

# Design philosophy

**Reliable**

- *Explicit units for output*
  - Numeric output of a function will have a specified unit attribute (if applicable) using the [units](https://r-quantities.github.io/units/) package. Use `units::drop_units(x)` to drop units.
- *Fail-fast without assumptions*
  - If certain conditions are not explicitly provided, err on the side of caution (e.g. a fasting state has to be provided in order for a glucose to be considered a fasting glucose). 
- *Explicit references and caveats*
  - Every function will have references and a section on caveats. (However, despite our best efforts of quality control, the functions are provided as-is, without any guarantees, and users would be wise to check the output for quality and accuracy in their use case.)

**Practical**
  
- *Input using units by expected use case*
  - Units used in the functions can be inconsistent (m versus cm, s versus ms) on purpose to best suit the most common usage of certain formulae.
- *Single entry functions if possible*
  - If multiple methods or formulae are known for the same phenomenon (e.g. body surface are), a single function is provided and the specific method can be specified as an argument. A default method is specified, if possible. This approach is not used if the parameters for functions differ.

# How to install

Clinicalr is not (yet) available on CRAN. Install devtools. Then use devtools to install clinicalr directly from GitHub using the [remotes](https://remotes.r-lib.org/) package.

```r
install.packages("remotes")
remotes::install_github("ls31/clinicalr")
```
# How to update

```r
remotes::install_github("lc31/clinicalr")
```
