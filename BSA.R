#' Calculates BSA (body surface area) (Mosteller, 1987).
#' @param weight Weight in kg.
#' @param length Length in cm.
#' @return BSA in m^2.
calculateBSA = function(weight, length) {
  bsa = ((weight * length) / 3600) ^ (1 / 2)
  return(bsa)
}
