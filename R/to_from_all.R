# helper functions for converters
# (need to be adjusted if new classes are added)





.supported.classes <- c("ts", "mts", "xts", "data.frame", "data.table", "tbl_df", "tsibble", "tbl", "dts", "tslist")

#' Extract Relevant Class
#'
#' Mainly used internally.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @examples
#' relevant_class(AirPassengers)
#' relevant_class(ts_df(AirPassengers))
#' @export
relevant_class <- function(x) {
  if (inherits(x, "dts")) {
    return("dts")
  }
  if (inherits(x, "ts")) {
    return("ts")
  }
  if (inherits(x, "xts")) {
    return("xts")
  }
  if (inherits(x, "data.table")) {
    return("data.table")
  }
  if (inherits(x, "tbl_ts")) {
    return("tsibble")
  }
  if (inherits(x, "tbl")) {
    return("tbl")
  }
  if (inherits(x, "data.frame")) {
    return("data.frame")
  }
  if (inherits(x, "tslist")) {
    return("tslist")
  }
  return("")
}


#' Test if an Object is ts-Boxable
#'
#' Mainly used internally.
#'
#' @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @return logical, either `TRUE` or `FALSE`
#' @examples
#' ts_boxable(AirPassengers)
#' ts_boxable(lm)
#' @export
ts_boxable <- function(x) {
  relevant_class(x) %in% .supported.classes
}


# Universal Converter Function
#
# @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
# @return returns a function
as_class <- function(x) {
  stopifnot(x %in% .supported.classes)
  get(paste0("ts_", x))
}



desired_class <- function(ll) {
  z <- unique(vapply(ll, relevant_class, ""))
  if (length(z) == 1) {
    if (z == "ts") {
      # no "ts" if mixed frequecies
      if (length(unique(vapply(ll, frequency, 1))) > 1) return("data.frame")
    }
    return(z)
  } else {
    # first non ts class
    return(z[z != "ts"])
  }
}



#' Re-Class ts-Boxable Object
#'
#' Copies class attributes from an existing ts-boxable series. Mainly used
#' internally.
#'
#' Inspired by `xts::reclass`, which does something similar.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`. Object to re-class.
#' @param template ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`. Template.
#' @param preserve.mode should the mode the time column be preserved (data frame only)
#' @param preserve.names should the name of the time column be preserved (data frame only)
#' @param preserve.time should the values time column be preserved (data frame only)
#' @export
copy_class <- function(x, template, 
                       preserve.mode = TRUE, 
                       preserve.names = TRUE, 
                       preserve.time = FALSE) {

  if (!ts_boxable(x)) {
    if (inherits(template, "ts")) {
      x <- ts(x)
      tsp(x) <- tsp(template)
    } else if (mode(x) == "numeric") {
      x.ts <- ts_ts(template)
      x <- ts(x)
      tsp(x) <- tsp(x.ts)
    } else {
      # do not reclass non numeric, unknown objects
      return(x)
    }
  }

  # to deal with 1 period time series: separate ts treatment
  if (inherits(template, "ts")){
    if (inherits(x, "ts")){
      ans <- x
    } else {
      x.dts <- ts_dts(x)
      # is there only one observation?
      if (number_of_series(x.dts) == nrow(ts_dts(x.dts))){
        ans <- ts_ts_dts(ts_dts(x), frequency = frequency(template))
      } else {
        ans <- as_class(relevant_class(template))(x)
      }
    }
  } else {
    ans <- as_class(relevant_class(template))(x)
  }

  # data frames may keep mode, names or time stamps
  if (inherits(ans, "data.frame") && inherits(template, "data.frame")) {
    tn <- guess_time(ans)
    if (preserve.mode) {
      if ((class(ans[[tn]])[1] == "Date") && (class(template[[tn]])[1] == "POSIXct")) {
        ans[[tn]] <- as.POSIXct(ans[[tn]])
      }
    }

    if (preserve.time) {
      ans[[tn]] <- template[[tn]]
    }

    if (preserve.names) {
      if (!identical(names(ans), names(template))){
        names(ans) <- names(template)
      }
    }

  }

  ans
}
