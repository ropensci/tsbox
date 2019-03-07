register_class("tslist")

# to ---------------------------------------------------------------------------

ts_tslist_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  if (number_of_series(x) == 1) {
    z <- list(ts_ts(x))
  } else {
    x <- combine_id_cols(x)
    cid <- dts_cname(x)$id
    spl <- split(x, x[[cid]])
    spl <- spl[unique(x[[cid]])]
    z <- lapply(spl, ts_ts)
  }
  class(z) <- c("tslist", "list")
  z
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts tslist
ts_dts.tslist <- function(x) {
  ll <- lapply(x, ts_dts)
  if (length(ll) > 1) {
    z <- rbindlist(ll, idcol = "id")
  } else {
    z <- ll[[1]]
  }
  ts_dts(z)
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tslist <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tslist") return(x)
  ts_tslist_dts(ts_dts(x))
}
