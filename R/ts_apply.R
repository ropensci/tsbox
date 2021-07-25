# ts_apply works on dts, so it may cause quite a bit of overhead if applied,
# e.g. on a ts object.

# fun can rely on time and value colum beeing called 'time' and 'value'

ts_apply_dts <- function(x, fun, ...) {
  .SD <- NULL
  stopifnot(inherits(x, "dts"))
  d <- dts_default(x); x <- d$x
  if (number_of_series(x) == 1) {
    z <- fun(x, ...)
    # ensure id columns are preserved
    missing.cid <- setdiff(colnames(x), colnames(z))
    if (length(missing.cid) > 0) {
      for (i in missing.cid) {
        z[[i]] <- unique(x[[i]])
      }
    }
  } else {
    .by <- by_expr(dts_cname(x)$id)
    # modifiy cname, to reflect single series character of .SD
    cname.sd <- dts_cname(x)
    cname.sd$id <- character(0)
    setattr(x, "cname", cname.sd)
    z <- x[, fun(.SD, ...), by = eval(.by)]
  }
  dts_restore(z, d)
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

