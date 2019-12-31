#' Estimate the mean arterial pressure (MAP)
#'
#' The mean arterial pressure is estimated using the formula:
#' \eqn{\text{MAP} \approxeq P_{\text{diastolic}} + \frac{1}{3} (P_{\text{systolic}} - P_{\text{diastolic}})}.
#'
#' @references None.
#'
#' @section Caveats: None at this time.
#'
#' @param sbp Systolic blood pressure (mmHg).
#' @param dbp Diastolic blood pressure (mmHg).
#' @return MAP (mmHg).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
estimate_map <- function(sbp, dbp) {
  assertthat::assert_that(assertthat::is.number(sbp) | is.na(sbp))
  assertthat::assert_that(assertthat::is.number(dbp) | is.na(dbp))
  (dbp + (1/3 * (sbp - dbp))) %>%
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
#' @param sbp Systolic blood pressure (mmHg).
#' @param dbp Diastolic blood pressure (mmHg).
#' @return Pulse pressure (mmHg).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
calculate_pulse_pressure <- function(sbp, dbp) {
  assertthat::assert_that(assertthat::is.number(sbp) | is.na(sbp))
  assertthat::assert_that(assertthat::is.number(dbp) | is.na(dbp))
  (sbp - dbp) %>%
    units::set_units("mmHg", mode = "standard")
}
