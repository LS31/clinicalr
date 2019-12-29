#' Calculate the mean arterial pressure (MAP).
#'
#' The mean arterial pressure is determined for 1/3 by systolic and 2/3 by diastolic blood pressure.
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
calculate_map <- function(systolic_blood_pressure, diastolic_blood_pressure) {
  assertthat::assert_that(is.numeric(systolic_blood_pressure))
  assertthat::assert_that(is.numeric(diastolic_blood_pressure))
  ((1/3 * systolic_blood_pressure) + (2/3 * diastolic_blood_pressure)) %>%
    units::set_units("mmHg", mode = "standard")
}
