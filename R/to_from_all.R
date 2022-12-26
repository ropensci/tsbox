.tsbox_registry <- new.env(parent = emptyenv())

#' Register a Time Series Class
#'
#' So tsbox is aware of it. Run when tsbox is loaded.
#'
#' @param tsbox.class character, class name, as used by tsbox
#' @param actual.class character, the actual class name, which may be different.
#'   E.g., tsbox calls tibbles `tbl`, so it can convert time series through
#'   `ts_tbl()`, while their actual name is `"tbl_df"`.
#' @noRd
register_class <- function(tsbox.class, actual.class = tsbox.class) {
  class <- setNames(actual.class, tsbox.class)
  classes <- c(class, .tsbox_registry$class)
  # unique() that keep names
  classes <- classes[unique(names(classes))]
  assign("class", classes, envir = .tsbox_registry)
}


# register dts class
register_class("dts")


#' Supported Classes
#'
#' Returns all classes that have been registered.
#' @examples
#' supported_classes()
#' @noRd
supported_classes <- function() {
  .tsbox_registry$class
}


#' Extract Relevant Class
#'
#' Mainly used internally.
#'
#' @inherit ts_default
#' @return character, the relevant class of ts-boxable object
#' @examples
#' relevant_class(AirPassengers)
#' x <- ts_df(AirPassengers)
#' relevant_class(x)
#' @export
relevant_class <- function(x) {
  check_ts_boxable(x)
  intersect(class(x), supported_classes())[1]
}


#' Test if an Object is ts-Boxable
#'
#' Mainly used internally.
#'
#' @inherit ts_default
#' @return logical, either `TRUE` or `FALSE`. `check_ts_boxable()` fails if not
#'   `TRUE`
#' @examples
#' ts_boxable(AirPassengers)
#' ts_boxable(lm)
#' @export
ts_boxable <- function(x) {
  any(supported_classes() %in% class(x))
}

#' Error Check Functions
#'
#' @inherit ts_default
#'
#' @name ts_boxable
#' @export
check_ts_boxable <- function(x) {
  if (!ts_boxable(x)) {
    stop0(
      "object is of non-ts-boxable class(es) ",
      paste(paste0("'", class(x), "'"), collapse = ", "),
      ". See `?ts_ts`."
    )
  }
}


#' Universal Converter Function
#'
#' @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @return returns a function
#' @noRd
as_class <- function(x) {
  stopifnot(any(supported_classes() %in% x))

  x.tsbox <- names(supported_classes())[match(x, supported_classes())]
  getFromNamespace(paste0("ts_", x.tsbox), ns = "tsbox")
}


#' Desired Class
#'
#' If different time series objects are combined in `ts_c()`, `ts_bind()` etc.,
#' which class should be returned
#'
#' @param ll a list with time series objects, usually collected via list(...)
#' @return character string
#' @examples
#' desired_class(list(mdeaths, ts_df(fdeaths)))
#' @noRd
desired_class <- function(ll) {
  z <- unique(vapply(ll, relevant_class, ""))
  if (length(z) == 1L) {
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
#' @inherit ts_default
#' @param template ts-boxable time series, an object of class `ts`, `xts`,
#'   `zoo`, `zooreg`, `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`,
#'   `tis`, `irts` or `timeSeries`.
#' @param preserve.mode should the mode the time column be preserved
#'   (data frame only)
#' @param preserve.names should the name of the time column be preserved
#'   (data frame only)
#' @param preserve.time should the values time column be preserved
#'   (data frame only)
#' @param preserve.attr should the attributes of the value column be preserved
#'   (data frame only)
#' @return a ts-boxable object of the same class as `template`,
#'   i.e., an object of class `ts`, `xts`, `zoo`,
#'   `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`, `irts` or
#'   `timeSeries`.
#' @export
#' @examples
#' copy_class(mdeaths, ts_tbl(fdeaths))
copy_class <- function(x, template,
                       preserve.mode = TRUE,
                       preserve.names = FALSE,
                       preserve.time = FALSE,
                       preserve.attr = TRUE) {
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
  if (inherits(template, "ts")) {
    if (inherits(x, "ts")) {
      ans <- x
    } else {
      x.dts <- ts_dts(x)
      # is there only one observation?
      if (number_of_series(x.dts) == nrow(ts_dts(x.dts))) {
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
      if (
        (class(ans[[tn]])[1] == "Date") &&
          (class(template[[tn]])[1] == "POSIXct")
      ) {
        ans[[tn]] <- as.POSIXct(ans[[tn]])
      }
    }

    if (preserve.time) {
      ans[[tn]] <- template[[tn]]
    }

    if (preserve.attr) {
      vn <- guess_value(ans)
      attributes(ans[[vn]]) <- attributes(template[[vn]])
    }

    if (preserve.names) {
      if (!identical(names(ans), names(template))) {
        names(ans) <- names(template)
      }
    }
  }

  ans
}
