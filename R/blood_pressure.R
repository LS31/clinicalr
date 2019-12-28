#' Calculate the mean arterial pressure (MAP).
#'
#' Mean arterial pressure = 1/3(SBP) + 2/3(DBP).
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
calculate_map <- function(sbp, dbp) {
  ((1/3 * sbp) + (2/3 * dbp)) %>%
    units::set_units("mmHg", mode = "standard")
}
