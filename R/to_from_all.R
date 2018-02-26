# helper functions for converters
# (need to be adjusted if new classes are added)

.supported.classes <- c("ts", "mts", "xts", "data.frame", "data.table", "tbl_df", "tbl", "dts", "tslist")


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
  class(x)[1] %in% .supported.classes
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

#' Extract the Relevant Class
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
  if (inherits(x, "tbl")) {
    return("tbl")
  }
  if (inherits(x, "data.frame")) {
    return("data.frame")
  }
  if (inherits(x, "tslist")) {
    return("tslist")
  }
  stop("not a ts_boxable object.")
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
copy_class <- function(x, template, preserve.mode = TRUE, preserve.names = TRUE, 
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
  ans <- as_class(relevant_class(template))(x)



  # data frames should keep mode of time col.
  if (preserve.mode && 
      inherits(ans, "data.frame") && 
      inherits(template, "data.frame")) {
    tn <- guess_time(ans)
    if ((class(ans[[tn]])[1] == "Date") && (class(template[[tn]])[1] == "POSIXct")) {
      ans[[tn]] <- as.POSIXct(ans[[tn]])
    }
  }

  if (preserve.time && 
    inherits(ans, "data.frame") && 
    inherits(template, "data.frame")) {
    tn <- guess_time(ans)
    ans[[tn]] <- template[[tn]]
  }

  if (preserve.names && 
      inherits(ans, "data.frame") && 
      inherits(template, "data.frame")) {
    if (!identical(names(ans), names(template))){
      names(ans) <- names(template)
    }
  }

  ans
}
