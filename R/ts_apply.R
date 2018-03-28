# ts_apply works on dts, so it may cause quite a bit of overhead if applied,
# e.g. on a ts object.

ts_apply_dts <- function(x, fun, ...) {
  stopifnot(inherits(x, "dts"))
  if (number_of_series(x) == 1) return(fun(x, ...))

  cname <- dts_cname(x)
  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))

  # modifiy cname, to reflect single series character of .SD
  cname.sd <- cname
  cname.sd$id <- character(0)
  setattr(x, "cname", cname.sd)
  z <- x[, fun(.SD), by = eval(.by)]
  setattr(z, "cname", cname)
  dts_init(z)
}

# ts_apply(ts_c(mdeaths, fdeaths), ts_diff)
#' @export
#' @inherit ts_dts
#' @param ... arguments passed to subfunction
#' @name ts_
ts_apply <- function(x, fun, ...) {
  stopifnot(ts_boxable(x))
  z <- ts_apply_dts(ts_dts(x), fun, ...)
  copy_class(z, x)
}
