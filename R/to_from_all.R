# helper functions for converters
# (need to be adjusted if new classes are added)



.tsbox_registry <- new.env(parent = emptyenv())

register_class <- function(tsbox.class, actual.class = tsbox.class){
  class <- setNames(actual.class, tsbox.class)
  classes <- c(class, .tsbox_registry$class)
  # to keep names
  classes <- classes[classes == unique(classes)]
  assign("class", classes, envir = .tsbox_registry)
}

register_class("dts")

supported_classes <- function(){
  .tsbox_registry$class
}


#' Extract Relevant Class
#'
#' Mainly used internally.
#'
#' @inherit ts_dts
#' @examples
#' relevant_class(AirPassengers)
#' relevant_class(ts_df(AirPassengers))
#' @export
relevant_class <- function(x) {
  stopifnot(ts_boxable(x))
  intersect(class(x), supported_classes())[1]
}


#' Test if an Object is ts-Boxable
#'
#' Mainly used internally.
#'
#' @inherit ts_dts
#' @return logical, either `TRUE` or `FALSE`
#' @examples
#' ts_boxable(AirPassengers)
#' ts_boxable(lm)
#' @export
ts_boxable <- function(x) {
  any(supported_classes() %in% class(x))
}


# Universal Converter Function
#
# @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
# @return returns a function
as_class <- function(x) {
  stopifnot(any(supported_classes() %in% x))

  x.tsbox <- names(supported_classes())[match(x, supported_classes())]
  getFromNamespace(paste0("ts_", x.tsbox), ns = "tsbox")
}



desired_class <- function(ll) {
  z <- unique(vapply(ll, relevant_class, ""))
  if (length(z) == 1) {
    if (z == "ts") {
      # no "ts" if mixed high frequecies
      uf <- unique(vapply(ll, frequency, 1))
      if ((length(uf) > 1) && (max(uf) > 12)) {
        return("data.frame")
      }
    }
    return(z)
  } else {
    # first non ts class
    return(z[z != "ts"][1])
  }
}



#' Re-Class ts-Boxable Object
#'
#' Copies class attributes from an existing ts-boxable series. Mainly used
#' internally.
#'
#' Inspired by `xts::reclass`, which does something similar.
#'
#' @inherit ts_dts
#' @param template ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`. Template.
#' @param preserve.mode should the mode the time column be preserved (data frame only)
#' @param preserve.names should the name of the time column be preserved (data frame only)
#' @param preserve.time should the values time column be preserved (data frame only)
#' @export
copy_class <- function(x, template,
                       preserve.mode = TRUE,
                       preserve.names = FALSE,
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
