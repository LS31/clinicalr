#' Estimate the mean arterial pressure (MAP)
#'
#' The mean arterial pressure is estimated using the formula:
#' \eqn{\text{MAP} \approxeq P_{\text{diastolic}} + \frac{1}{3} (P_{\text{systolic}} - P_{\text{diastolic}})}.
#'
#' @references None.
#'
#' @section Caveats: None at this time.
#'
#' @param systolic_blood_pressure Systolic blood pressure (mmHg).
#' @param diastolic_blood_pressure Diastolic blood pressure (mmHg).
#' @return MAP (mmHg).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
estimate_mean_arterial_pressure <- function(systolic_blood_pressure, diastolic_blood_pressure) {
  assertthat::assert_that(assertthat::is.number(systolic_blood_pressure) | is.na(systolic_blood_pressure))
  assertthat::assert_that(assertthat::is.number(diastolic_blood_pressure) | is.na(diastolic_blood_pressure))
  (diastolic_blood_pressure + (1/3 * (systolic_blood_pressure - diastolic_blood_pressure))) %>%
    units::set_units("mmHg", mode = "standard")
}

#' Calculate pulse pressure
#'
#' The pulse pressure is calculated using the formula: \eqn{P_\text{pulse} =
#' P_{\text{systolic}} - P_{\text{diastolic}}}.
#'
#' @references None.
#'
#' @section Caveats: None at this time.
#'
#' @param systolic_blood_pressure Systolic blood pressure (mmHg).
#' @param diastolic_blood_pressure Diastolic blood pressure (mmHg).
#' @return Pulse pressure (mmHg).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
calculate_pulse_pressure <- function(systolic_blood_pressure, diastolic_blood_pressure) {
  assertthat::assert_that(assertthat::is.number(systolic_blood_pressure) | is.na(systolic_blood_pressure))
  assertthat::assert_that(assertthat::is.number(diastolic_blood_pressure) | is.na(diastolic_blood_pressure))
  (systolic_blood_pressure - diastolic_blood_pressure) %>%
    units::set_units("mmHg", mode = "standard")
}
