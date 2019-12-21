#' Calculate the corrected QT interval (QTc).
#'
#' Calculate the corrected QT interval (QTc) of a electrocardiogram, according to Bazett (Bazett, 1920) or Fridericia (Fridericia, 1920).
#' 
#' @param qt QT interval in ms.
#' @param heart_rate Heart rate in beats/min.
#' @param method Formula for QTc: Fridericia (default) or Bazzett.
#' @return QTc in ms
#' @export
calculate_qtc <- function(qt, heart_rate, method = c("Fridericia", "Bazzett")) {
  if (method == "Fridericia") {
    calculate_qtc_fridericia(qt, heart_rate)
  } else if (method == "Bazett") {
    calculate_qtc_bazett(qt, heart_rate)
  } else {
    stop("Illegal value for argument method in calculate_qtc().")
  }
}

#' @describeIn calculate_qtc QTc according to Bazett.
#' @keywords internal
calculate_qtc_bazett <- function(qt, heart_rate) {
  qt <- qt / 1000
  rr <- 60 / heart_rate
  qt / (rr ^ (1 / 2)) * 1000
}

#' @describeIn calculate_qtc QTc according to Fridericia.
#' @keywords internal
calculate_qtc_fridericia <- function(qt, heart_rate) {
  qt <- qt / 1000
  rr <- 60 / heart_rate
  qt / (rr ^ (1 / 3)) * 1000
}

