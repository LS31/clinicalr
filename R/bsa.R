#' Calculate the body surface area (BSA).
#'
#' Calculate the body surface area (BSA), according to Monsteller (see
#' \url{https://doi.org/10.1056\%2FNEJM198710223171717}) or Du Bois (see
#' \url{https://doi.org/10.1001\%2Farchinte.1916.00080130010002}).
#'
#' @param weight Weight in kg.
#' @param height Height in cm.
#' @param method Formula to calculate BSA, either Monsteller (default) or Du Bois.
#'   (default).
#' @return BSA in m^2.
#' @export
calculate_bsa <- function(weight, height, method = c("Monsteller", "Du Bois")) {
  if (method == "Monsteller") {
    calculate_bsa_monsteller(weight, height)
  } else if (method == "Du Bois") {
    calculate_bsa_du_bois(weight, height)
  } else {
    stop("Illegal value for argument method in calculate_bsa().")
  }
}

#' @describeIn calculate_bsa Calculate the body surface area (BSA) according to Monsteller.
#' @keywords internal
calculate_bsa_monsteller <- function(weight, height) {
  (weight * height / 3600) ^ (0.5)
}

#' @describeIn calculate_bsa Calculate the body surface area (BSA) according to Du Bois
#' @keywords internal
calculate_bsa_du_bois <- function(weight, height) {
  0.007184 * (weight ^ 0.425) * (height ^ 0.725)
}
