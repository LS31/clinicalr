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
#' @return Albumin-adjusted calcium level (mmol/l), or `NA` if any parameters are `NA`.
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
adjust_calcium_for_albumin <-
  function(calcium, albumin, normal_albumin = 40) {
    if (anyNA(c(calcium, albumin, normal_albumin))) {
      return(NA)
    }
    assertthat::assert_that(assertthat::is.number(calcium))
    assertthat::assert_that(assertthat::is.number(albumin))

    (calcium + 0.025 * (normal_albumin - albumin)) %>%
      units::set_units("mmol1 l-1", mode = "standard")
  }


#' Adjust sodium for hyperglycaemia
#'
#' Hyperglycaemia can lead to hypertonic hyponatriemia. Using observational
#' data, the following formula predicts sodium levels after correction of
#' glucose: \eqn{\text{Na}_{predicted} = \text{Na}_\text{measured} + \kappa
#' \times \frac{\text{glucose} - 5.6}{5.6}}, where \eqn{\kappa} is 1.6 when
#' derived from the publication by Katz and 2.4 when derived from more recent
#' work by Hillier (default).
#'
#' @references [Katz, M. A. Hyperglycemia-Induced Hyponatremia — Calculation of
#'   Expected Serum Sodium Depression. N Engl J Med 289, 843–844
#'   (1973).](https://doi.org/10.1056/NEJM197310182891607) and [Hillier, T. A.,
#'   Abbott, R. D. & Barrett, E. J. Hyponatremia: evaluating the correction
#'   factor for hyperglycemia. The American Journal of Medicine 106, 399–403
#'   (1999).](https://doi.org/10.1016/s0002-9343(99)00055-8).
#'
#' @section Caveats: It's just a prediction, and the choice between both
#'   formulae may result in quite different predictions.
#'
#' @param sodium Measured sodium level (mmol/l).
#' @param glucose Measured glucose level (mmol/l).
#' @param method Formula. Options are: "Hillier" (default), "Katz".
#' @return Glucose-adjusted sodium level (mmol/l), or `NA` if any parameters are `NA`.
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
adjust_sodium_for_glucose <-
  function(sodium, glucose, method = "Hillier") {
    if (anyNA(c(sodium, glucose, method))) {
      return(NA)
    }
    assertthat::assert_that(assertthat::is.number(sodium))
    assertthat::assert_that(assertthat::is.number(glucose))
    assertthat::assert_that(assertthat::is.string(method))

    xkappa <- switch(
      method,
      "Hillier" = 2.4,
      "Katz" = 1.6,
      stop(paste0("Error: `method` must be `Hillier` or `Katz`.\n You've supplied method `", method, "`."))
    )

    (sodium + xkappa * ((glucose-5.6)/5.6)) %>%
      units::set_units("mmol1 l-1", mode = "standard")
}
