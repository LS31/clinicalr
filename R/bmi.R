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
#' @return BMI (\eqn{kg m^-2}).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
calculate_bmi <- function(weight, height) {
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
#' @references \url{https://doi.org/10.1177/106002807400801104}.
#'
#' @section Caveats: This provides estimates for adults only.
#'
#' @param height Height (cm).
#' @param is_female \code{TRUE} if patient is female, \code{FALSE} if patient is
#'   male.
#' @return IBW (kg).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
estimate_ideal_body_weight <- function(height, is_female) {
  assertthat::assert_that(assertthat::is.flag(is_female))
  assertthat::assert_that(assertthat::is.number(height) | is.na(height))

  if(is_female) {
    (45.5 + 0.9 (height - 152)) %>%
      units::set_units("kg", mode = "standard")
  } else {
    return (50 + 0.9 (height - 152))  %>%
      units::set_units("kg", mode = "standard")
  }
}
