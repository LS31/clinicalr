#' Determine presence of metabolic syndrome.
#'
#' Determines whether an individual has the metabolic syndrome based on the
#' definition of the NCEP ATPIII criteria, updated in 2005 by Grundy et al.
#'
#' Please note: we DO include use of a statin as a drug for HDL cholesterol and
#' triglicerides, even though the original statement is unclear about this
#' point (only including niacin and fibrates).
#'
#' Please note: only glucose needs to be from a fasting blood sample (and not
#' triglycerides or HDL cholesterol). This is as defined in the NCEP ATPIII
#' criteria. The glucose trait is determined in a fail-fast way: if you do not
#' explicitly set the is_fasting_blood_sample to TRUE, the glucose measurement is
#' considered to be non-fastening and is ignored.
#'
#' This function explicitly handles situations of missing data. For example,
#' with blood pressure 120/80 mmHg and antihypertensive medication, a patient is
#' still considered hypertensive. With blood pressure NA/90 mmHg without
#' antihypertensive medication, a patient is still considered hypertensive. A
#' patient with blood pressure NA/NA mmHg and no antihypertensive medication is
#' considered NA for the trait hypertension, but if a patient has NA/NA mmHg but
#' does use antihypertensive medication, the patient is considered to have
#' hypertension (even though measurements are missing).
#'
#' @param is_female TRUE if patient is female, FALSE if patient is a man.
#' @param waist_circumference Waist circumference in cm.
#' @param systolic_blood_pressure Systolic blood pressure in mmHg.
#' @param triglycerides Triglycerides in mmol/l.
#' @param diastolic_blood_pressure Systolic blood pressure in mmHg.
#' @param hdl_cholesterol HDL cholesterol in mmol/l.
#' @param glucose Glucose in mmol/l.
#' @param is_fasting_blood_sample TRUE if fasting blood sample, FALSE if not or
#'   unknown. By default FALSE, and glucose levels then be treated as missing
#'   (i.e. fail-fast by default).
#' @param has_antihypertensive_drug TRUE if patient is on antihypertensive drug
#'   treatment, otherwise FALSE (default).
#' @param has_lipid_drug TRUE if patient is on fibrate, nicotinic acid or statin.
#' @param has_glucose_drug TRUE if patient is on drug treatment for elevated
#'   glucose.
#'
#' @return TRUE if patient has the metabolic syndrome, FALSE is not, NA if
#'   indetermined.
has_metabolic_syndrome <- function(is_female,
                                  waist_circumference = NA,
                                  systolic_blood_pressure = NA,
                                  diastolic_blood_pressure = NA,
                                  triglycerides = NA,
                                  hdl_cholesterol = NA,
                                  glucose = NA,
                                  is_fasting_blood_sample = FALSE,
                                  has_antihypertensive_drug = FALSE,
                                  has_lipid_drug = FALSE,
                                  has_glucose_drug = FALSE) {
  stopifnot(!is.na(is_female))

  # Define cutoff values
  # For upper limits, the cutoff itself is considered as too high.
  # For lower limits, the cutoff itself is considered to be normal.
  max_triglycerides <- 1.7
  max_systolic_blood_pressure <- 130
  max_diastolic_blood_pressure <- 85
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

  if (!is.na(systolic_blood_pressure) || has_antihypertensive_drug) {
    if (systolic_blood_pressure >= max_systolic_blood_pressure ||
        has_antihypertensive_drug) {
      criteria[2] <- TRUE
    } else if (!is.na(diastolic_blood_pressure)) {
      if (diastolic_blood_pressure >= max_diastolic_blood_pressure) {
        criteria[2] <- TRUE
      } else if (!is.na(systolic_blood_pressure) &&
                 !is.na(diastolic_blood_pressure) &&
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
