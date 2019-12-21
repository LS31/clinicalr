#' Calculate the body mass index (BMI, Quetelet index).
#'
#' @param weight Weight in kg.
#' @param height Height in cm.
#' @return BMI in kg/m^2.
#' @export
calculate_bmi <- function(weight, height) {
  weight / ((height / 100) ^ 2)
}