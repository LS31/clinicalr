#' Adjust total calcium for albumin
#'
#' Most of the protein-bound calcium is bound to albumin. In hypoalbuminaemia
#' and hypocalcaemia, the (clinically relevant ionised) calcium level will be
#' underestimated. Therefore, total calcium levels may be adjusted/corrected for
#' albumin. An alternative approach is measuring ionised calcium levels
#' directly. Calcium is adjusted using the formula:
#' \eqn{\text{calcium}_\text{measured} + 0.025 (\text{albumin}_\text{normal} -
#' \text{albumin}_\text{measured}))}.
#'
#' @references [Bushinsky, D. A. & Monk, R. D. Calcium. The Lancet 352, 306–311 (1998).](https://doi.org/10.1016/S0140-6736(97)12331-5).
#'
#' @section Caveats: Adjustments for albumin are quite poor substitutes for
#'   measuring ionised calcium.
#'
#' @param calcium Measured total calcium level (mmol/l).
#' @param albumin Measured albumin level (g/l).
#' @param normal_albumin Normal value of albumin (g/l) (40 by default).
#' @return Albumin-adjusted calcium level (mmol/l).
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
adjust_calcium_for_albumin <-
  function(calcium, albumin, normal_albumin = 40) {
    assertthat::assert_that(assertthat::is.number(calcium) |
                              is.na(calcium))
    assertthat::assert_that(assertthat::is.number(albumin) |
                              is.na(albumin))

    (calcium + 0.025 * (normal_albumin - albumin)) %>%
      units::set_units("mmol1 l-1", mode = "standard")
  }
