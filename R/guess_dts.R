#' Guess Time Attribute
#'
#' @param x a 'dts'
#'
#' - called by dts accessors, it attribute is not yet present.
#' - can assume x is dts, not test needed
#' - same name as dts accessor
#' - should never manipulate x
#'
#' @examples
#' guess_tattr(ts_dts(mdeaths))
#' @noRd
guess_tattr <- function(x) {
  x.time <- x[[dts_cname(x)$time]]
  class <- class(x.time)[1]
  if (!(class %in% c("Date", "POSIXct"))) {
    stop0("[time] column is not of class 'Date' or 'POSIXct'")
  }
  if (identical(class, "POSIXct")) {
    tz <- attr(x.time, "tzone")
  } else {
    tz <- ""
  }
  list(
    class = class,
    tz = tz
  )
}


#' Guess Column Names
#'
#' @param x a 'dts'
#'
#' - called by dts accessors, it attribute is not yet present.
#' - can assume x is dts, not test needed
#' - same name as dts accessor
#' - should never manipulate x
#'
#' @examples
#' guess_cname(ts_dts(mdeaths))
#' @noRd
guess_cname <- function(x) {
  value.name <- guess_value(x)
  time.name <- guess_time(x, value.name = value.name)

  msg <- NULL
  if (time.name != "time") {
    msg <- paste0("[time]: '", time.name, "' ")
  }
  if (value.name != "value") {
    msg <- paste0(msg, "[value]: '", value.name, "' ")
    # check if data frame is incidentally wide (numeric id columns)
    non_value <- setdiff(colnames(x), value.name)
    numeric.id.cols <- vapply(x[, non_value, with = FALSE], is.numeric, TRUE)
    if (sum(numeric.id.cols) > 0) {
      message(
        "Found numeric [id] column(s): ",
        paste_quoted(names(numeric.id.cols)[numeric.id.cols]),
        ".\nAre you using a wide data frame? To convert, use 'ts_long()'.",
        "\nConvert column(s) to character or factor to silence this message.\n"
      )
    }
  }

  if (!is.null(msg)) message(msg)

  list(
    id = setdiff(colnames(x), c(time.name, value.name)),
    time = time.name,
    value = value.name
  )
}
