# - called by dts accessors, it attribute is not yet present.
# - can assume x is dts, not test needed
# - same name as dts accessor
# - should never manipulate x


guess_tattr <- function(x){
  x.time <- x[[dts_cname(x)$time]]
  class <- class(x.time)[1]
  if (!(class %in% c("Date", "POSIXct"))) {
    stop("[time] col is not of class 'Date' or 'POSIXct'", call. = FALSE)
  }
  if (class == "POSIXct"){
    tz <- attr(x.time, 'tzone')
  } else {
    tz = ""
  }
  list(
    class = class, 
    tz = tz
  )
}

guess_cname <- function(x) {
  value.name <- guess_value(x)
  time.name <- guess_time(x, value.name = value.name)

  msg <- NULL
  if (time.name != "time") {
    msg <- paste0("[time]: '", time.name, "' ")
  }
  if (value.name != "value") {
    msg <- paste0(msg, "[value]: '", value.name, "' ")
    # check if data frame is incidentally wide
    cnames <- colnames(x)
    cols.right.of.time <- cnames[(which(cnames == time.name) + 1):length(cnames)]
    if (length(cols.right.of.time) > 1){
      value.cols <- vapply(x[, cols.right.of.time, with = FALSE], is_value, TRUE)
      if (sum(value.cols) > 1){
        message(
        "More than one value column detected after the time colum, using the outermost.\n",
        "Are you using a wide data frame? ",
        "To convert, use 'ts_long'.\n"
        )
      }
    }
  }

  if (!is.null(msg)) message(msg)

  list(
    id = setdiff(colnames(x), c(time.name, value.name)),
    time = time.name,
    value = value.name
  )
}
