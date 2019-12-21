#' Calculate the body surface area (BSA).
#'
#' Calculate the body surface area (BSA), according to Monsteller or Du Bois.
#' 
#' @references
#' Monsteller: \url{https://doi.org/10.1056\%2FNEJM198710223171717};
#' Du Bois: \url{https://doi.org/10.1001\%2Farchinte.1916.00080130010002}.
#' 
#' @section Caveats:
#' None at this time.
#'
#' @param weight Weight (kg).
#' @param height Height (cm).
#' @param method Formula to calculate BSA. Options are: "Monsteller" (default), "Du Bois".
#' @return BSA (\eqn{m^2}).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
calculate_bsa <- function(weight, height, method = "Monsteller") {
  switch(method,
         "Monsteller" = calculate_bsa_monsteller(weight, height),
         "Du Bois" = calculate_bsa_du_bois(weight, height),
         stop("Illegal value for argument method in calculate_bsa()."))
}

#' @describeIn calculate_bsa Calculate the body surface area (BSA) according to Monsteller.
calculate_bsa_monsteller <- function(weight, height) {
  x = (weight * height / 3600) ^ (0.5)
  units::set_units(x, "m-2", mode = "standard")
}

#' @describeIn calculate_bsa Calculate the body surface area (BSA) according to Du Bois
calculate_bsa_du_bois <- function(weight, height) {
  x = 0.007184 * (weight ^ 0.425) * (height ^ 0.725)
  units::set_units(x, "m-2", mode = "standard")
}
