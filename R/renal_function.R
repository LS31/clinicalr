#' Estimate creatinine clearance and glomerular filtration rate using the Cockcroft-Gault formula
#'
#' The formula is \eqn{\frac{(140 - \text{age}) \times \text{weight}}{0.81 \times \text{creatinine}} \times S}, where \eqn{S} is 1 for males and 0.85 for females.
#'
#' @references [Cockcroft, D. W. & Gault, H. Prediction of Creatinine Clearance from Serum Creatinine. Nephron 16, 31–41 (1976).](https://doi.org/10.1159/000180580)
#'
#' @section Caveats:
#' Creatinine clearance may overestimate glomerular filtration rate in kidney injury. In non-ideal body composition, Cockcroft-Gault formula may be (increasingly) inaccurate. Note that the eGFR provided by this formula is _not_ normalised for a standardised body surface area of 1.73 \eqn{m^2} - to achieve this, divide the eGFR by the patients body surface area (using your method of choice) and multiply by 1.73 \eqn{m^2}.
#'
#' @param creatinine Creatinine (\eqn{\text{μmol} \text{l}^{-1}}).
#' @param age Age (years).
#' @param is_female `TRUE` if patient is female, `FALSE` if patient is
#'   male.
#' @param weight Weight (kg).
#' @return eGFR (\eqn{\text{ml} \text{min}^{-1}}).
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
estimate_gfr_cockcroft <- function(creatinine, age, is_female, weight) {
  assertthat::assert_that(assertthat::is.number(creatinine) |
                            is.na(creatinine))
  assertthat::assert_that(assertthat::is.number(age) |
                            is.na(age))
  assertthat::assert_that(assertthat::is.flag(is_female))
  assertthat::assert_that(assertthat::is.number(weight) |
                            is.na(weight))

  egfr <- ((140 - age) * weight) / (0.81 * creatinine)

  if (is_female) {
    egfr <- egfr * 0.85
  }

  egfr %>%
    units::set_units("ml1 min-1", mode = "standard")
}

#' Estimate creatinine clearance and glomerular filtration rate using the MDRD formula
#'
#' The MDRD formula is: \eqn{175 \times (\frac{\text{creatinine}}{88.4})^{-1.154} \times (\text{age})^{-0.203} \times S \times A}, where \eqn{S} is 1 for males and 0.742 for females and \eqn{A} is 1.212 for African-Americans and 1 for non-African-Americans. This is the so-called 4-variable version (as opposed to the 6-variable version).
#'
#' @references [Levey, A. S. A More Accurate Method To Estimate Glomerular Filtration Rate from Serum Creatinine: A New Prediction Equation. Ann Intern Med 130, 461 (1999).](https://doi.org/10.7326/0003-4819-130-6-199903160-00002) and [Levey, A. S. et al. Using Standardized Serum Creatinine Values in the Modification of Diet in Renal Disease Study Equation for Estimating Glomerular Filtration Rate. Ann Intern Med 145, 247 (2006). ](https://doi.org/10.7326/0003-4819-145-4-200608150-00004)
#'
#' @section Caveats:
#' Note that this formula returns an eGFR for a standardised body surface area of 1.73 \eqn{m^2}. The MDRD formula may underestimate actual glomerular filtration rate in healthy patients. The original publication used 186 as a constant, but this was later revised to 175 provided the laboratory had calibrated its serum creatinine measurements to isotope dilution mass spectrometry (and therefore, 175 is used here). The factor 'African-American' was published as black/non-black - I do not know if this is an acceptable/usable generalisation for all people with a black skin tone (or for people of other non-Caucasian, non-African-American descent for that matter).
#'
#' @param creatinine Creatinine (\eqn{\text{μmol} \text{l}^{-1}}).
#' @param age Age (years).
#' @param is_female `TRUE` if patient is female, `FALSE` if patient is
#'   male.
#' @param is_african_american `TRUE` if patient is African-American, `FALSE` if patient is not.
#' @return eGFR (\eqn{\text{ml} \text{min}^{-1} 1.73 \text{m}^{-2}}).
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
estimate_gfr_mdrd <- function(creatinine, age, is_female, is_african_american) {
  assertthat::assert_that(assertthat::is.number(creatinine) | is.na(creatinine))
  assertthat::assert_that(assertthat::is.number(age) | is.na(age))
  assertthat::assert_that(assertthat::is.flag(is_female))
  assertthat::assert_that(assertthat::is.flag(is_african_american))

  egfr <- 175 * (creatinine / 88.4) ^ -1.154 * (age)^-0.203

  if (is_female) {
    egfr <- egfr * 0.742
  }

  if (is_african_american) {
    egfr <- egfr * 1.212
  }

  egfr %>%
    units::set_units("ml1 min-1", mode = "standard")
}

#' Estimate creatinine clearance and glomerular filtration rate using the Schwartz formula
#'
#' The Schwartz formula is used in children: \eqn{\frac{36.2 \times \text{height}}{\text{creatinine}}}.
#'
#' @references [Schwartz, G. J. & Work, D. F. Measurement and Estimation of GFR in Children and Adolescents. CJASN 4, 1832–1843 (2009).](https://doi.org/10.2215/CJN.01640309)
#'
#' @section Caveats:
#' This is the revised (2009) formula with the same factor \eqn{\kappa} for all age categories: 36.2. The returned value is based on a standardised body surface area of 1.73 \eqn{m^2}.
#'
#' @param creatinine Creatinine (\eqn{\text{μmol} \text{l}^{-1}}).
#' @param height Height (cm).
#' @return eGFR (\eqn{\text{ml} \text{min}^{-1} 1.73 \text{m}^{-2}}).
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
estimate_gfr_schwartz <- function(creatinine, height) {
  assertthat::assert_that(assertthat::is.number(creatinine) | is.na(creatinine))
  assertthat::assert_that(assertthat::is.number(height) | is.na(height))
  (36.2 * height) / creatinine %>%
    units::set_units("ml1 min-1", mode = "standard")
}

#' Estimate creatinine clearance and glomerular filtration rate using the
#' CKD-EPI formula
#'
#' The CKD-EPI formula is calculated in two stages. First, \eqn{Z =
#' \frac{\text{creatinine}}{\kappa}}, where \eqn{\kappa} is 61.9 for females and 79.6 for males.
#' Then, if \eqn{Z < 1}, use \eqn{141 \times Z^(\alpha) \times 1^{-1.209} \times
#' 0.993^{\text{age}} \times S \times A}, where \eqn{\alpha} is -0.329 in females and -0.411 in
#' males, \eqn{S} is 1.018 in females and 1 in males, and \eqn{A} is 1.159 for
#' African-Americans and 1 for non-African-Americans. However, if \eqn{Z > 1},
#' use \eqn{141 \times 1^{\alpha} \times Z^{-1.209} \times 0.993^{\text{age}} \times S
#' \times A}, where \eqn{\alpha} is -0.329 in females and -0.411 in males, \eqn{S} is 1.018
#' in females and 1 in males, and \eqn{A} is 1.159 for African-Americans and 1 for
#' non-African-Americans.
#'
#' @references [Levey, A. S. et al. A New Equation to Estimate Glomerular Filtration Rate. Ann Intern Med 150, 604 (2009).](https://doi.org/10.7326/0003-4819-150-9-200905050-00006)
#'
#' @section Caveats: This is the formula for when cystatine C values are
#'   unavailable. The factor 'African-American' was published as black/non-black.
#'   I do not know if this is an acceptable/usable generalisation for all
#'   people with a black skin tone (or for people of other non-Caucasian,
#'   non-African-American descent for that matter).
#'
#' @param creatinine Creatinine (\eqn{\text{μmol} \text{l}^{-1}}).
#' @param age Age (years).
#' @param is_female `TRUE` if patient is female, `FALSE` if patient is
#'   male.
#' @param is_african_american `TRUE` if patient is African-American,
#'   `FALSE` if patient is not.
#' @return eGFR (\eqn{\text{ml} \text{min}^{-1} 1.73 \text{m}^{-2}}).
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
estimate_gfr_ckdepi <- function(creatinine, age, is_female, is_african_american) {
  assertthat::assert_that(assertthat::is.number(creatinine) | is.na(creatinine))
  assertthat::assert_that(assertthat::is.number(age) | is.na(age))
  assertthat::assert_that(assertthat::is.flag(is_female))
  assertthat::assert_that(assertthat::is.flag(is_african_american))

  xkappa <- ifelse(is_female, 61.9, 79.6)
  xalpha <- ifelse(is_female, -0.329, -0.411)
  xmin <- ifelse(creatinine/xkappa < 1, creatinine/xkappa, 1)
  xmax <- ifelse(creatinine/xkappa > 1, creatinine/xkappa, 1)

  egfr = 141 * xmin^xalpha * xmax^-1.209 * 0.993^age

  if (is_female) {
    egfr <- egfr * 1.018
  }

  if (is_african_american) {
    egfr <- egfr * 1.159
  }

  egfr %>%
    units::set_units("ml1 min-1", mode = "standard")
}
