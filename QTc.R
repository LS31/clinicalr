#' Calculates QT based on Bazett and Fridericia formula.
#' @param qt QT interval in ms.
#' @param heart_rate Heart rate in beats/min.
#' @return QTc in ms in a two dimension array (Bazett and Fredericia).
calculate_qtc <- function(qt, heart_rate) {
  qt <- qt / 1000
  rr <- 60 / heart_rate
  qtc <- NULL
  qtc$bazett <- qt / (rr ^ (1 / 2)) * 1000
  qtc$fridericia <- qt / (rr ^ (1 / 3)) * 1000
  return(qtc)
}
