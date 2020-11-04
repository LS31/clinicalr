#' Calculate the corrected QT interval (QTc)
#'
#' Calculate the corrected QT interval (QTc) of a electrocardiogram, according
#' to Bazett (Bazett, 1920) or Fridericia (Fridericia, 1920).
#'
#' @param qt QT interval (ms).
#' @param heart_rate Heart rate (/min).
#' @param method Formula for QTc. Options are: "Fridericia" (default),
#'   "Bazzett".
#' @return QTc (ms).
#' @export
#' @seealso [units::set_units()], [units::drop_units()]
calculate_qtc <- function(qt, heart_rate, method = "Fridericia") {
  switch(method,
         "Fridericia" = calculate_qtc_fridericia(qt, heart_rate),
         "Bazett" = calculate_qtc_bazett(qt, heart_rate),
         stop("Illegal value for argument method in calculate_qtc()."))
}

#' @describeIn calculate_qtc QTc according to Bazett.
#' @noRd
calculate_qtc_bazett <- function(qt, heart_rate) {
  assertthat::assert_that(assertthat::is.number(qt) | is.na(qt))
  assertthat::assert_that(assertthat::is.number(heart_rate) | is.na(heart_rate))
  qt <- qt / 1000
  rr <- 60 / heart_rate
  (qt / (rr ^ (1 / 2)) * 1000) %>%
    units::set_units("ms", mode = "standard")
}

#' @describeIn calculate_qtc QTc according to Fridericia.
#' @noRd
calculate_qtc_fridericia <- function(qt, heart_rate) {
  assertthat::assert_that(assertthat::is.number(qt) | is.na(qt))
  assertthat::assert_that(assertthat::is.number(heart_rate) | is.na(heart_rate))
  qt <- qt / 1000
  rr <- 60 / heart_rate
  (qt / (rr ^ (1 / 3)) * 1000) %>%
    units::set_units("ms", mode = "standard")
}

