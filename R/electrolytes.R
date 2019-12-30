#' Correct calcium for albumin level
#'
#' Most of the protein-bound calcium is bound to albumin. Lower albumin will lower total calcium levels, but the ionised calcium level will not be lowered that much. Therefore, total calcium levels should be corrected for albumin, since in a state of
#' hypoalbuminemia the (ionised, relevant) calcium level is underestimated. An alternative approach
#' is measuring ionised calcium levels directly.
#'
#' The formula used here is (calcium + 0.025 * (normal albumin - measured albumin)).
#'
#' @references Calcium (Bushinsky; 1998): \url{https://doi.org/10.1016/S0140-6736(97)12331-5}.
#'
#' @section Caveats: Corrections for albumin are quite poor substitutes for measuring ionised calcium.
#'
#' @param calcium Measured (uncorrected) total calcium level (mmol/l).
#' @param albumin Measured albumin level (g/l).
#' @param normal_albumin Normal value of albumin (g/l) (40 by default).
#' @return Albumin-corrected calcium level (mmol/l).
#' @export
#' @seealso \code{\link[units]{set_units}}, \code{\link[units]{drop_units}}
correct_calcium_for_albumin <- function(calcium, albumin, normal_albumin = 40) {
  assertthat::assert_that(assertthat::is.number(calcium) | is.na(calcium))
  assertthat::assert_that(assertthat::is.number(albumin) | is.na(albumin))

  (calcium + 0.025 * (normal_albumin - albumin)) %>%
    units::set_units("mmol1 l-1", mode = "standard")
}
