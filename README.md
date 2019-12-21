# CliniCalc: Clinical calculations for R
This is a collection of R functions to perform (basic) clinical calculations in medical research.

The functions are provided as-is. Pull requests are more than welcome.

Current functions are:

- Anthropometry
  - Body mass index (BMI, Quetelet index).
  - Body surface area (BSA), according to Monsteller or Du Bois.
- Cardiology
  - Corrected QT interval (QTc) from a electrocardiogram, according to Bazett (Bazett, 1920) or Fridericia (Fridericia, 1920).
- Metabolism  
  - Presence of the metabolic syndrome, according to the [updated NCEP ATPIII criteria (Grundy, 2005)](http://www.ncbi.nlm.nih.gov/pubmed/16157765).

# Philosophy

- Practical units: units used in the functions are inconsistent (m versus cm, s versus ms) on purpose, to best suit the most common usage of certain formulae.
- Single entry function: if multiple methods or formulae are known for the same phenomenon (e.g. body surface are), a single function is provided and the specific method can be specified as an argument. As a consequence, the package is opinionated: we specify a default method, if possible. This approach is not implemented if the parameters for functions vary to much.
- Fail-fast: if certain conditions are not explicitly provided, err on the side of caution (e.g. a fasting state has to be provided in order for a glucose to be considered a fasting glucose). 

# How to install

CliniCalc is not (yet) available on CRAN. Install devtools. Then use devtools to install CliniCalc directly from GitHub.

```{r}
install.packages("devtools")
devtools::install_github("ls31/clinicalcalculations")
```
# How to update

```{r}
devtools::install_github("lc31/clinicalcalculations")
```
