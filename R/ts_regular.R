#' Enforce Regularity
#'
#' Enforces regularity in data frame and `xts` objects, by turning implicit
#' `NA`s into explicit `NA`s. In `ts` objects, regularity is automatically
#' enforced.
#'
#' @param x a ts-boxable time series
#' @examples
#' x0 <- AirPassengers
#' x0[c(10, 15)] <- NA
#' x <- ts_na_omit(ts_dts(x0))
#' ts_regular(x)
#'
#' m <- mdeaths
#' m[c(10, 69)] <- NA
#' f <- fdeaths
#' f[c(1, 3, 15)] <- NA
#'
#' ts_regular(ts_na_omit(ts_dts(ts_c(f, m))))
#' @export
ts_regular <- function(x) {
  stopifnot(ts_boxable(x))
  z <- regular_core(ts_dts(x))
  copy_class(z, x)
}

# A basic test for regularity. Fast, but misses some regular series
is_regular_one_basic <- function(x) {
  if (length(x) == 1) return(TRUE)
  rng <- range(diff(as.numeric(x)))
  (rng[2] - rng[1]) < 1
}

regular_core <- function(x) {
  stopifnot(inherits(x, "dts"))

  cname <- dts_cname(x)
  ctime <- cname$time
  cid <- cname$id
    # browser()

  regular_core_one <- function(x) {
    if (is_regular_one_basic(x[[ctime]])) return(x)
    reg.time <- regularize_date(x[[ctime]])
    if (is.null(reg.time)) {
      stop("series has no regular pattern", call. = FALSE)
    }
    merge_time_date(data.table(time = reg.time), x, by.x = "time", by.y = ctime)
  }

  if (length(cid) == 0) {
    z <- regular_core_one(x)
  } else {
    .by <- parse(text = paste0("list(", paste(cid, collapse = ", "), ")"))
    z <- x[, regular_core_one(.SD), by = eval(.by)]
  }

  setattr(z, "cname", cname)
  
  # resulting time column name should be ctime
  setnames(z, "time", ctime)

  # preserve original col order
  setcolorder(z, names(x))
  z
}
