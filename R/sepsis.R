#' Score qSOFA in sepsis
#'
#' The quick SOFA (Sequential Organ Failure Assessment) score is used to estimate mortality in sepsis.
#'
#' @references [Seymour, C. W. et al. Assessment of Clinical Criteria for Sepsis. JAMA 315, 762 (2016).](https://doi.org/10.1001/jama.2016.0288).
#'
#' @section Caveats: Note that the original publications used GCS < 14 as
#'   'altered mental state' but the current
#'   [qSOFA website](https://qsofa.org/what.php) promotes GCS < 15 as
#'   cutoff. Here, GCS   < 15 is used as 'altered mental state'. Note that the
#'   actual GCS does not matter for the qSOFA: if you only know if there was a
#'   altered metal state, use `gcs = 15` for normal metal state and for
#'   example `gcs = 1` for any altered metal state.
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

# TODO Score SOFA in sepsis
#
# The Sequential Organ Failure Assessment (SOFA) score is used to estimate
# mortality in sepsis in the setting of an intensive care unit.
#
# @references Use of the SOFA Score to Assess the Incidence of Organ Dysfunction/Failure in Intensive Care Units: Results of a Multicenter, Prospective Study. Working Group on "Sepsis-Related Problems" of the European Society of Intensive Care Medicine. (Vincent et al., 1998). \url{https://doi.org/10.1097/00003246-199811000-00016}.
#
# @section Caveats: Mechanical ventilation for causes other that respiratory dysfunction (e.g. neurological disorders) should not be counted in the SOFA score. In sedated patients, use the assumed GCS (in absence of iatrogenic sedation) instead of the actual GCS.
# score_sofa <- function(pao2,
#                        fio2,
#                        has_mechanical_ventilation,
#                        trombocytes,
#                        gcs,
#                        bilirubin,
#                        map,
#                        dopamine_dose,
#                        dobutamine_dose,
#                        epinephrine_dose,
#                        norepinephrine_dose,
#                        creatinine,
#                        urine_output) {
#   assertthat::assert_that(assertthat::is.number(gcs))
#   assertthat::assert_that(assertthat::is.number(fio2))
#   assertthat::assert_that(assertthat::is.flag(has_mechanical_ventilation))
#   assertthat::assert_that(assertthat::is.number(gcs))
#   assertthat::assert_that(assertthat::is.number(bilirubin))
#   assertthat::assert_that(assertthat::is.number(map))
#   assertthat::assert_that(assertthat::is.number(dopamine_dose))
#   assertthat::assert_that(assertthat::is.number(dobutamine_dose))
#   assertthat::assert_that(assertthat::is.number(epinephrine_dose))
#   assertthat::assert_that(assertthat::is.number(norepinephrine_dose))
#   assertthat::assert_that(assertthat::is.number(creatinine))
#   assertthat::assert_that(assertthat::is.number(urine_output))
#
#   score <- 0
#
#   stop("To do")
#
#   return(score)
# }
