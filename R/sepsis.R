#' Score qSOFA in sepsis
#'
#' This quick SOFA score is used to estimate mortality in sepsis.
#'
#' @references Assessment of Clinical Criteria for Sepsis. For the Third
#'   International Consensus Definitions for Sepsis and Septic Shock (Sepsis-3)
#'   (Seymour et al., 2016). \url{https://doi.org/10.1001/jama.2016.0288}.
#'
#' @section Caveats: Note that the original publications used GCS < 14 as
#'   'altered mental state' but the current \url[qSOFA
#'   website]{https://qsofa.org/what.php} promotes GCS < 15 as cutoff. Here, GCS
#'   < 15 is used as 'altered mental state'. Note that the actual GCS does not
#'   matter for the qSOFA: if you only know if there was a altered metal state,
#'   use \code{gcs = 15} for normal metal state and for example \code{gcs = 1}
#'   for any altered metal state.
#'
#' @param gcs Glasgow Coma Scale.
#' @param respiratory_rate Respiratory rate (/min).
#' @param sbp Systolic blood pressure (mmHg).
#' @return qSOFA score (points).
#' @export
score_qsofa <- function(gcs,
                        respiratory_rate,
                        sbp) {
  assertthat::assert_that(assertthat::is.number(gcs))
  assertthat::assert_that(assertthat::is.number(respiratory_rate))
  assertthat::assert_that(assertthat::is.number(sbp))

  score <- 0

  if (gcs < 15) {
    score <- score + 1
  }

  if (respiratory_rate >= 22) {
    score <- score + 1
  }

  if (sbp <= 100) {
    score <- score + 1
  }

  return(score)
}
