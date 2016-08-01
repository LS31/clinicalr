#' Determines whether an individual has the metabolic syndrome based on the 
#' definition of the NCEP ATPIII criteria, updated in 2005 by Grundy et al.
#' 
#' Please note: we DO include use of a statin as a drug for HDL cholesterol and 
#' triglicerides, even though the original statements is unclear about this 
#' point (only including niacin and fibrates).
#' 
#' Please note: only glucose needs to be a from a fasting blood sample (and not
#' triglycerides or HDL cholesterol). This is as defined in the NCEP ATPIII
#' criteria. The glucose trait is determined in a fail-fast way: if you do not
#' explicitly set the isFastingBloodSample to TRUE, the glucose measurement is
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
#' @param isFemale TRUE if patient is female, FALSE if patient is a man.
#' @param waistCircumference Waist circumference in cm.
#' @param systolicBloodPressure Systolic blood pressure in mmHg.
#' @param triglycerides Triglycerides in mmol/l.
#' @param diastolicBloodPressure Systolic blood pressure in mmHg.
#' @param hdlCholesterol HDL cholesterol in mmol/l.
#' @param glucose Glucose in mmol/l.
#' @param isFastingBloodSample TRUE if fasting blood sample, FALSE if not or 
#'   unknown. By default FALSE, and glucose levels then be treated as missing 
#'   (i.e. fail-fast by default).
#' @param hasAntihypertensiveDrug TRUE if patient is on antihypertensive drug 
#'   treatment, otherwise FALSE (default).
#' @param hasLipidDrug TRUE if patient is on fibrate, nicotinic acid or statin.
#' @param hasGlucoseDrug TRUE if patient is on drug treatment for elevated 
#'   glucose.
#'   
#' @return TRUE if patient has the metabolic syndrome, FALSE is not, NA if 
#'   indetermined.
hasMetabolicSyndrome = function(isFemale,
                                waistCircumference = NA,
                                systolicBloodPressure = NA,
                                diastolicBloodPressure = NA,
                                triglycerides = NA,
                                hdlCholesterol = NA,
                                glucose = NA,
                                isFastingBloodSample = FALSE,
                                hasAntihypertensiveDrug = FALSE,
                                hasLipidDrug = FALSE,
                                hasGlucoseDrug = FALSE) {
  
  # Define cutoff values
  # For upper limits, the cutoff itself is considered as too high.
  # For lower limits, the cutoff itself is considered to be normal.
  maxTriglycerides = 1.7
  maxSystolicBloodPressure = 130
  maxDiastolicBloodPressure = 85
  maxGlucose = 5.6
  if (isFemale) {
    maxWaistCircumference = 88
    minHdlCholesterol = 1.3
    
  } else if (!isFemale) {
    maxWaistCircumference = 102
    minHdlCholesterol = 1.03
  }
  
  # Check for traits. Ugly but nicely verbose. Consider every trait NA by default.
  criteria = c(NA, NA, NA, NA, NA)
  names(criteria) = c("waistCircumference", "bloodPressure", "triglycerides", "hdlCholesterol", "glucose")

  if (!is.na(waistCircumference)) {
    if (waistCircumference >= maxWaistCircumference) {
      criteria[1] = TRUE
    } else if (waistCircumference < maxWaistCircumference) {
      criteria[1] = FALSE
    }
  }
  
  if (!is.na(systolicBloodPressure) || hasAntihypertensiveDrug) {
    if (systolicBloodPressure >= maxSystolicBloodPressure || hasAntihypertensiveDrug) {
      criteria[2] = TRUE
    } else if (!is.na(diastolicBloodPressure)) {
      if (diastolicBloodPressure >= maxDiastolicBloodPressure) {
        criteria[2] = TRUE
      } else if (!is.na(systolicBloodPressure) && !is.na(diastolicBloodPressure) && !hasAntihypertensiveDrug) {
        criteria[2] = FALSE
      }
    }
  }

  if (!is.na(triglycerides) || hasLipidDrug) {
    if (triglycerides >= maxTriglycerides || hasLipidDrug) {
        criteria[3] = TRUE
    } else if (triglycerides < maxTriglycerides && !hasLipidDrug) {
      criteria[3] = FALSE
    }
  }
  
  if(!is.na(hdlCholesterol) || hasLipidDrug) {
    if (hdlCholesterol < minHdlCholesterol || hasLipidDrug) {
     criteria[4] = TRUE
    } else if (hdlCholesterol >= minHdlCholesterol && !hasLipidDrug) {
     criteria[4] = FALSE
    }
  }
  
  if(!is.na(glucose) || hasGlucoseDrug) {
    if ((glucose >= maxGlucose && isFastingBloodSample) || hasGlucoseDrug) {
      criteria[5] = TRUE
    } else if (glucose < maxGlucose && isFastingBloodSample && !hasGlucoseDrug) {
      criteria[5] = FALSE
    }
  }
  
  # print(criteria)
  # return(criteria)
  
  # Finally, count criteria and make a decision.
  if (sum(criteria, na.rm = TRUE) >= 3) {
    # >= 3 traits, therefore metabolic syndrome.
    return(TRUE)
  } else if (sum(!criteria, na.rm = TRUE) >= 3) {
    # >= 3 traits not there, therefore no metabolic syndrome, regardless of missing traits.
    return(FALSE)
  } else {
    # Indetermined.
    return(NA)
  }
}
