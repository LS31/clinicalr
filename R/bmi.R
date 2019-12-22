#' Calculate the body mass index (BMI, Quetelet index).
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
  (weight / ((height / 100) ^ 2)) %>%
    units::set_units("kg1 m-2", mode = "standard")
}
