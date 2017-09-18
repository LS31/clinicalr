#' Calculate the corrected QT intevral (QTc).
#'
#' Calculate the corrected QT interval (QTc) of a electrocardiogram, according to Bazett (Bazett, 1920) or Fridericia (Fridericia, 1920).
#' @param qt QT interval in ms.
#' @param heart_rate Heart rate in beats/min.
#' @return QTc in ms
#' @rdname calculate_qtc
calculate_qtc_bazett <- function(qt, heart_rate) {
  qt <- qt / 1000
  rr <- 60 / heart_rate
  qt / (rr ^ (1 / 2)) * 1000
}

#' @rdname calculate_qtc
calculate_qtc_fridericia <- function(qt, heart_rate) {
  qt <- qt / 1000
  rr <- 60 / heart_rate
  qt / (rr ^ (1 / 3)) * 1000
}
