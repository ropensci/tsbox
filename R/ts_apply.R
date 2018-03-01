# ts_apply works on dts, so it may cause quite a bit of overhead if applied,
# e.g. on a ts object.

ts_apply_dts <- function(x, fun, ...) {
  stopifnot(inherits(x, "dts"))
  if (number_of_series(x) == 1) return(fun(x, ...))

  cid <- dts_cname(x)$id
  .by <- parse(text = paste0("list(", paste(cid, collapse = ", "), ")"))
  z <- x[, fun(.SD), by = eval(.by)]
  browser()
  dts_init(z)
}

# ts_apply(ts_c(mdeaths, fdeaths), ts_diff)
ts_apply <- function(x, fun, ...) {
  stopifnot(ts_boxable(x))
  z <- ts_apply_dts(ts_dts(x), fun, ...)
  copy_class(z, x)
}
