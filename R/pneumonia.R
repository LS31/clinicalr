#' Score community-acquired pneumonia severity using CURB-65
#'
#' The CURB-65 community-acquired pneumonia severity score is used to estimate
#' mortality.
#'
#' @references [Lim, W. S. Defining community acquired pneumonia severity on presentation to hospital: an international derivation and validation study. Thorax 58, 377–382 (2003).](https://doi.org/10.1136/thorax.58.5.377).
#'
#' @section Caveats: None at this time.
#'
#' @param confusion Mental confusion (`TRUE` or `FALSE`).
#' @param urea  Blood urea nitrogen (mmol/l).
#' @param respiratory_rate Respiratory rate (/min).
#' @param sbp Systolic blood pressure (mmHg).
#' @param dbp Diastolic blood pressure (mmHg).
#' @param age Age (years).
#' @return CURB-65 score (points), or `NA` if any parameters are `NA`.
#' @export
score_curb65 <- function(confusion,
                         urea,
                         respiratory_rate,
                         sbp,
                         dbp,
                         age) {
  if(anyNA(c(confusion, urea, respiratory_rate, sbp, dbp, age))){
    return(NA)
  }
  assertthat::assert_that(assertthat::is.flag(confusion))
  assertthat::assert_that(assertthat::is.number(urea))
  assertthat::assert_that(assertthat::is.number(respiratory_rate))
  assertthat::assert_that(assertthat::is.number(sbp))
  assertthat::assert_that(assertthat::is.number(dbp))
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

  if (sbp < 90 | dbp <= 60) {
    score <- score + 1
  }

  if (age >= 65) {
    score <- score + 1
  }

  return(score)
}
