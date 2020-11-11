#' Calculate the body mass index (BMI, Quetelet index)
#'
#' Body mass index is calculated using the formula: \eqn{\text{BMI} =
#' \text{weight} / \text{height}^2} with weight in kg en height in m.
#'
#' @references None.
#'
#' @section Caveats: None at this time.
#'
#' @param weight Weight (kg).
#' @param height Height (cm).
#' @return BMI (\eqn{\text{kg} m^{-2}}), or `NA` if any parameters are `NA`.
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
calculate_bmi <- function(weight, height) {
  if (anyNA(c(weight, height))) {
    return(NA)
  }
  assertthat::assert_that(assertthat::is.number(weight) |
                            is.na(weight))
  assertthat::assert_that(assertthat::is.number(height) |
                            is.na(height))

  (weight / ((height / 100) ^ 2)) %>%
    units::set_units("kg1 m-2", mode = "standard")
}

#' Estimate ideal body weight (IBW)
#'
#' Ideal body weight is estimated using the Devine formulae:
#' for males \eqn{\text{IBW} = 50 + 0.9 (\text{height} - 152)} and
#' for females \eqn{\text{IBW} = 45.5 + 0.9 (\text{height} - 152)}
#' with IBW in kg en height in m.
#'
#' @references [McCarron, M. M. & Devine, B. J. Clinical Pharmacy: Case Studies. Drug Intelligence & Clinical Pharmacy 8, 650–655 (1974).](https://doi.org/10.1177/106002807400801104)
#'
#' @section Caveats: This provides estimates for adults only.
#'
#' @param height Height (cm).
#' @param is_female `TRUE` if patient is female, `FALSE` if patient is
#'   male.
#' @return IBW (kg), or `NA` if any parameters are `NA`.
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
estimate_ibw <- function(height, is_female) {
  if (anyNA(c(height, is_female))) {
    return(NA)
  }
  assertthat::assert_that(assertthat::is.number(height))
  assertthat::assert_that(assertthat::is.flag(is_female))

  if(is_female) {
    (45.5 + 0.9 * (height - 152)) %>%
      units::set_units("kg1", mode = "standard")
  } else {
    (50 + 0.9 * (height - 152))  %>%
      units::set_units("kg1", mode = "standard")
  }
}
