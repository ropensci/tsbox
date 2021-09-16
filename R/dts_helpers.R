#' Create a Data Table Based Time Series Object
#'
#' @param x a `data.table` object
#' @noRd
#' @srrstats {TS1.5} *The software should ensure strict ordering of the time, frequency, or equivalent ordering index variable.*
#'   Sorts if not already ordered.
#' @srrstats {TS1.6} *Any violations of ordering should be caught in the pre-processing stages of all functions.*
#'   Done here.
dts_init <- function(x) {
  .SD <- NULL
  stopifnot(inherits(x, "data.frame"))
  x <- as.data.table(x)
  stopifnot(inherits(x, "data.table"))

  is_list_col <- vapply(x, function(e) "list" %in% class(e), TRUE)
  if (any(is_list_col)) {
    stop0("'x' contains list columns, which are not yet supported.")
  }

  setattr(x, "class", c("dts", attr(x, "class")))
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)

  # do not allow duplicates
  is.dup <- duplicated(x[, c(cname$id, cname$time), with = FALSE])
  if (any(is.dup)) {
    z <- as.data.frame(unique(x[is.dup, cname$id, with = FALSE]))
    paste_ <- function(...) paste(..., sep = "_")
    dups <- do.call(paste_, as.list(z))
    if (length(dups) > 0) {
      stop0(
        "object contains series with duplicated information: ",
        paste(dups, collapse = ", ")
      )
    } else {
      stop0(
        "series contains duplicated values in time column: ",
        unique(x[[cname$time]][duplicated(x[[cname$time]])])
      )
    }
  }
  if (!is.numeric(x[[cname$value]])) {
    stop0("'value' column [", cname$value, "] is not numeric.")
  }

  # new
  setnames(x, cname$time, "time")
  x[, time := as_time_or_date(time)]

  # ensure time is always ordered (if not done before)
  colorder <- names(x)
  .by <- by_expr(dts_cname(x)$id)
  x <- x[, setorder(.SD, time), by = eval(.by)]
  setcolorder(x, colorder)

  check_missing_time(x$time)

  setnames(x, "time", cname$time)
  setattr(x, "cname", cname)

  x
}


#' dts Helper: Remove dts attributes
#'
#' @param x 'dts'
#' @return a 'data.table'
#'
#' @noRd
dts_rm <- function(x) {
  setattr(x, "class", setdiff(attr(x, "class"), "dts"))
  setattr(x, "cname", NULL)
  setattr(x, "tattr", NULL)
  x
}


#' dts Helper: Extract (and optionally guess) Column Names
#'
#' Once guessed, they are added as an attribute, so cnames need to be guessed
#' only once.
#'
#' @param x 'dts'
#'
#' @noRd
dts_cname <- function(x) {
  stopifnot(inherits(x, "dts"))
  z <- attr(x, "cname")
  if (is.null(z)) {
    z <- guess_cname(x)
    setattr(x, "cname", z)
  }
  z
}


#' dts Helper: Extract (and optionally guess) Time Attribute
#'
#' Once guessed, they are added as an attribute, so cnames need to be guessed
#' only once.
#'
#' @param x 'dts'
#'
#' @noRd
dts_tattr <- function(x) {
  stopifnot(inherits(x, "dts"))
  z <- attr(x, "tattr")
  if (is.null(z)) {
    z <- guess_tattr(x)
    setattr(x, "tattr", z)
  }
  z
}


#' dts Helper: Determine the Number of Time Series
#'
#' @param x 'dts'
#'
#' @noRd
number_of_series <- function(x) {
  stopifnot(inherits(x, "dts"))
  cid <- dts_cname(x)$id
  if ((length(cid)) == 0L) {
    1
  } else {
    dt.id <- x[, cid, with = FALSE]
    nrow(unique(dt.id))
  }
}


#' dts Helper: Combine Several Id Columns into One
#'
#' Calls `combine_cols_data.table()`.
#'
#' @param x 'dts'
#'
#' @noRd
combine_id_cols <- function(x, sep = "_") {
  stopifnot(inherits(x, "dts"))
  if (NCOL(x) <= 3) {
    return(x)
  }
  cname <- dts_cname(x)
  z <- combine_cols_data.table(copy(x), dts_cname(x)$id, sep = sep)
  cname$id <- "id"
  setattr(z, "cname", cname)
  z
}
