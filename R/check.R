
stop0 <- function(...) {
  stop(..., call. = FALSE)
}

paste_quoted <- function(x) {
  paste(paste0("'", x,  "'"), collapse = ", ")
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

#' @param a character
#' @param b character
#'
#' @noRd
check_identical_ids <- function(a, b) {
  a <- sort(a)
  b <- sort(b)
  if (!identical(a, b)) {
    stop0(
      "[id] columns are not identical: ",
      paste(a, collapse = ", "),
      " (1); ",
      paste(b, collapse = ", "),
      " (2)"
    )
  }
}

#' @param x Date or POSIXct
#'
#' @noRd
check_missing_time <- function(x) {
  if (any(is.na(x))) {
    stop0("[time] column contains missing values")
  }
}

#' @param reg.time return value from regularize_date
#'
#' Fail if regularize_date() returns NULL
#'
#' @noRd
check_regular_pattern <- function(reg.time) {
  if (is.null(reg.time)) {
    stop0("series has no regular pattern")
  }
}


