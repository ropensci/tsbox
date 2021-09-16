
stop0 <- function(...) {
  stop(..., call. = FALSE)
}


#' Error Check Functions
#'
#' @param x 'dts'
#'
#' @noRd
check_ts_boxable <- function(x) {
  if (!ts_boxable(x)) {
    stop0(
      "object is of non-ts-boxable class(es) ",
      paste(paste0("'", class(x), "'"), collapse = ", "),
      ". See `?ts_ts`."
    )
  }
}


#' @param x 'dts'
#'
#' @noRd
check_frequency_detection <- function(x) {
  if (NROW(x) < 2) {
    stop0(
      "need at least two observations for frequency detection"
    )
  }
}


#' @param by numeric, or character
#'
#' @noRd
check_numeric_by <- function(by) {
  if (is.numeric(by)) {
    stop0(
      "'by' cannot be integer when used with irregular sequence"
    )
  }
}


#' @param by numeric, or character
#'
#' @noRd
check_start_end <- function(start, end) {
  if (start > end) {
    stop0(
      "'start' cannot be at or after 'end'"
    )
  }
}
