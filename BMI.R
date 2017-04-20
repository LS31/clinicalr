#' Calculates BMI.
#' @param weight Weight in kg.
#' @param length Length in cm.
#' @return BMI in kg/m^2.
calculate_bmi <- function(weight, length) {
  length <- length / 100
  weight / (length ^ 2)
}
