#' Determine presence of metabolic syndrome using updated NCEP ATPIII criteria
#'
#' Determines whether an individual has the metabolic syndrome based on the
#' definition of the NCEP ATPIII criteria, updated in 2005 by Grundy et al.
#'
#' @references Updated NCEP ATPIII criteria (Grundy et al., 2005):
#' \url{https://doi.org/10.1161/circulationaha.105.169404}.
#'
#' @section Caveats: We do include use of a statin as a drug for HDL cholesterol
#'   and triglicerides, even though the original statement is unclear about this
#'   point (only including niacin and fibrates).
#'
#'   According to the definition, only glucose needs to be from a fasting blood
#'   sample (and not triglycerides or HDL cholesterol). The glucose trait is
#'   determined in a fail-fast way: if you do not explicitly set
#'   \code{is_fasting_blood_sample = TRUE}, the glucose measurement is
#'   considered to be non-fastening and is ignored.
#'
#'   This function explicitly handles situations of missing data. For example,
#'   with blood pressure 120/80 mmHg and antihypertensive medication, a patient
#'   is still considered hypertensive. With blood pressure \code{NA}/90 mmHg
#'   without antihypertensive medication, a patient is still considered
#'   hypertensive. A patient with blood pressure \code{NA}/\code{NA} mmHg and no
#'   antihypertensive medication is considered \code{NA} for the trait
#'   hypertension, but if a patient has \code{NA}/\code{NA} mmHg but does use
#'   antihypertensive medication, the patient is considered to have hypertension
#'   (even though measurements are missing).
#'
#' @param is_female \code{TRUE} if patient is female, \code{FALSE} if patient is
#'   male.
#' @param waist_circumference Waist circumference (cm) (\code{NA} by default).
#' @param sbp Systolic blood pressure (mmHg) (\code{NA} by
#'   default).
#' @param triglycerides Triglycerides in (mmol/l) (\code{NA} by default).
#' @param dbp Systolic blood pressure (mmHg) (\code{NA} by
#'   default).
#' @param hdl_cholesterol HDL cholesterol (mmol/l) (\code{NA} by default).
#' @param glucose Glucose (mmol/l) (\code{NA} by default).
#' @param is_fasting_blood_sample \code{TRUE} if fasting blood sample,
#'   \code{FALSE} (default) if not or unknown.
#' @param has_antihypertensive_drug \code{TRUE} if patient is on
#'   antihypertensive drug treatment, otherwise \code{FALSE} (default).
#' @param has_lipid_drug \code{TRUE} if patient is on fibrate, nicotinic acid or
#'   statin, otherwise \code{FALSE} (default).
#' @param has_glucose_drug \code{TRUE} if patient is on drug treatment for
#'   elevated glucose, otherwise \code{FALSE} (default).
#' @return \code{TRUE} if patient has the metabolic syndrome, \code{FALSE} is
#'   not, \code{NA} if indetermined.
#' @export
has_metabolic_syndrome_atpiii <- function(is_female,
                                  waist_circumference = NA,
                                  sbp = NA,
                                  dbp = NA,
                                  triglycerides = NA,
                                  hdl_cholesterol = NA,
                                  glucose = NA,
                                  is_fasting_blood_sample = FALSE,
                                  has_antihypertensive_drug = FALSE,
                                  has_lipid_drug = FALSE,
                                  has_glucose_drug = FALSE) {
  assertthat::assert_that(assertthat::is.flag(is_female))
  assertthat::assert_that(assertthat::is.number(waist_circumference) | is.na(waist_circumference))
  assertthat::assert_that(assertthat::is.number(sbp) | is.na(sbp))
  assertthat::assert_that(assertthat::is.number(dbp) | is.na(dbp))
  assertthat::assert_that(assertthat::is.number(triglycerides) | is.na(triglycerides))
  assertthat::assert_that(assertthat::is.number(hdl_cholesterol) | is.na(hdl_cholesterol))
  assertthat::assert_that(assertthat::is.number(glucose) | is.na(glucose))
  assertthat::assert_that(assertthat::is.flag(is_fasting_blood_sample) | is.na(is_fasting_blood_sample))
  assertthat::assert_that(assertthat::is.flag(has_antihypertensive_drug) | is.na(has_antihypertensive_drug))
  assertthat::assert_that(assertthat::is.flag(has_lipid_drug) | is.na(has_lipid_drug))
  assertthat::assert_that(assertthat::is.flag(has_glucose_drug) | is.na(has_glucose_drug))

  # Define cutoff values
  # For upper limits, the cutoff itself is considered as too high.
  # For lower limits, the cutoff itself is considered to be normal.
  max_triglycerides <- 1.7
  max_sbp <- 130
  max_dbp <- 85
  max_glucose <- 5.6
  if (is_female) {
    maxwaist_circumference <- 88
    minhdl_cholesterol <- 1.3

  } else if (!is_female) {
    maxwaist_circumference <- 102
    minhdl_cholesterol <- 1.03
  }

  if (is.na(is_fasting_blood_sample)) {
    is_fasting_blood_sample <- FALSE
  }
  if (is.na(has_antihypertensive_drug)) {
    has_antihypertensive_drug <- FALSE
  }
  if (is.na(has_lipid_drug)) {
    has_lipid_drug <- FALSE
  }
  if (is.na(has_glucose_drug)) {
    has_glucose_drug <- FALSE
  }

  # Check for traits. Ugly but nicely verbose.
  # Consider every trait NA by default.
  criteria <- c(NA, NA, NA, NA, NA)
  names(criteria) <- c("waist_circumference",
                       "bloodPressure",
                       "triglycerides",
                       "hdl_cholesterol",
                       "glucose")

  if (!is.na(waist_circumference)) {
    if (waist_circumference >= maxwaist_circumference) {
      criteria[1] <- TRUE
    } else if (waist_circumference < maxwaist_circumference) {
      criteria[1] <- FALSE
    }
  }

  if (!is.na(sbp) || has_antihypertensive_drug) {
    if (sbp >= max_sbp ||
        has_antihypertensive_drug) {
      criteria[2] <- TRUE
    } else if (!is.na(dbp)) {
      if (dbp >= max_dbp) {
        criteria[2] <- TRUE
      } else if (!is.na(sbp) &&
                 !is.na(dbp) &&
                 !has_antihypertensive_drug) {
        criteria[2] <- FALSE
      }
    }
  }

  if (!is.na(triglycerides) || has_lipid_drug) {
    if (triglycerides >= max_triglycerides || has_lipid_drug) {
      criteria[3] <- TRUE
    } else if (triglycerides < max_triglycerides && !has_lipid_drug) {
      criteria[3] <- FALSE
    }
  }

  if (!is.na(hdl_cholesterol) || has_lipid_drug) {
    if (hdl_cholesterol < minhdl_cholesterol || has_lipid_drug) {
      criteria[4] <- TRUE
    } else if (hdl_cholesterol >= minhdl_cholesterol && !has_lipid_drug) {
      criteria[4] <- FALSE
    }
  }

  if (!is.na(glucose) || has_glucose_drug) {
    if ( (glucose >= max_glucose && is_fasting_blood_sample) ||
        has_glucose_drug) {
      criteria[5] <- TRUE
    } else if (glucose < max_glucose && is_fasting_blood_sample &&
               !has_glucose_drug) {
      criteria[5] <- FALSE
    }
  }

  # Finally, count criteria and make a decision.
  if (sum(criteria, na.rm = TRUE) >= 3) {
     # >= 3 traits, therefore metabolic syndrome.
     return(TRUE)
   } else if (sum(!criteria, na.rm = TRUE) >= 3) {
     # >= 3 traits not there, therefore no metabolic syndrome,
     # regardless of missing traits.
     return(FALSE)
   } else {
     # Indetermined.
     return(NA)
   }
}
