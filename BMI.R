#' Calculates BMI.
#' @param weight Weight in kg.
#' @param length Length in cm.
#' @return BMI in kg/m^2.
calculateBMI = function(weight, length) {
  length = length / 100
  bmi = weight / (length^2)
  return(bmi)
}
