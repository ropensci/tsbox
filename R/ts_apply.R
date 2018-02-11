# ts_apply works on dts, so it may cause quite a bit of overhead if applied,
# e.g. on a ts object.

ts_apply_dts <- function(x, fun, ...) {
  stopifnot(inherits(x, "dts"))
  if (number_of_series(x) == 1) return(fun(x, ...))
  colname.id <- colname_id(x)
  .by <- parse(text = paste0("list(", paste(colname.id, collapse = ", "), ")"))
  z <- x[, fun(.SD), by = eval(.by)]
  ts_dts(z)
}

# ts_apply(ts_c(mdeaths, fdeaths), ts_diff)
ts_apply <- function(x, fun, ...) {
  stopifnot(ts_boxable(x))
  z <- ts_apply_dts(ts_dts(x), fun, ...)
  copy_class(z, x)
}
