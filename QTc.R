#' Calculates QT based on Bazett and Fridericia formula.
#' @param QT QT interval in ms.
#' @param heartRate Heart rate in beats/min.
#' @return QTc in ms in a two dimension array (Bazett and Fredericia).
calculateQTc = function(QT, heartRate) {
  QT = QT / 1000
  RR = 60 / heartRate
  QTc = NULL
  QTc$Bazett = QT / (RR^(1/2)) * 1000
  QTc$Fridericia = QT / (RR^(1/3)) * 1000
  return(QTc)
}
