#' Calculate Body surface area (BSA).
#'
#' Calculate the body surface area (BSA), according to Monsteller (Monsteller, 1987).
#'
#' @param weight Weight in kg.
#' @param length Length in cm.
#' @return BSA in m^2.
#' @export
calculate_bsa <- function(weight, length) {
  (weight * length / 3600) ^ (1 / 2)
}
