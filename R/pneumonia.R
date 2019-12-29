#' Score community-acquired pneumonia severity using CURB-65.
#'
#' The CURB-65 community-acquired pneumonia severity score is used to estimate mortality.
#'
#' @references Defining community acquired pneumonia severity on presentation to hospital: an international derivation and validation study (Lim et al., 2003). \url{https://doi.org/10.1136/thorax.58.5.377}.
#'
#' @section Caveats: None at this time.
#'
#' @param confusion Mental confusion (TRUE or FALSE).
#' @param urea  Blood urea nitrogen (mmol/l).
#' @param respiratory_rate Respiratory rate (/min).
#' @param systolic_blood_pressure Systolic blood pressure (mmHg).
#' @param diastolic_blood_pressure Diastolic blood pressure (mmHg).
#' @param age Age (years).
#' @return CURB-65 score (points).
#' @export
score_curb65 <- function(confusion,
                          urea,
                          respiratory_rate,
                          systolic_blood_pressure,
                          diastolic_blood_pressure,
                          age) {
  assertthat::assert_that(assertthat::is.flag(confusion))
  assertthat::assert_that(assertthat::is.number(respiratory_rate))
  assertthat::assert_that(assertthat::is.number(systolic_blood_pressure))
  assertthat::assert_that(assertthat::is.number(diastolic_blood_pressure))
  assertthat::assert_that(assertthat::is.number(age))

  score <- 0

  if (confusion) {
    score <- score + 1
  }

  if (urea > 7) {
    score <- score + 1
  }

  if (respiratory_rate >= 30) {
    score <- score + 1
  }

  if (systolic_blood_pressure < 90 | diastolic_blood_pressure <= 60) {
    score <- score + 1
  }

  if (age >= 65) {
    score <- score + 1
  }

  return(score)
}
