register_class("tibbletime", "tbl_time")

# to ---------------------------------------------------------------------------

ts_tibbletime_dts <- function(x) {
  stopifnot(requireNamespace("tibbletime"))
  stopifnot(requireNamespace("tibble"))

  z <- wide_core(combine_id_cols(x))
  ctime <- dts_cname(x)$time
  tibbletime::as_tbl_time(z, index = !! ctime)
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts tbl_time
ts_dts.tbl_time <- function(x) {
  stopifnot(requireNamespace("tibbletime"))
  stopifnot(requireNamespace("tibble"))

  z <- as.data.table(x)
  time <- tibbletime::get_index_char(x)

  # clean up attributes
  setattr(z, "sorted", NULL)
  setattr(z, "index_quo", NULL)
  setattr(z, "index_time_zone", NULL)

  # simplified, single id melt, instead of ts_long,from ts_ts, ts_xts
  # could be factrored out.
  if (ncol(z) == 2) {
    names(z)[2] <- "value"
    setcolorder(z, c(time, "value"))
    id <- character(0)
  } else {
    z <- melt(z, id.vars = time, variable.name = "id", variable.factor = FALSE)
    setcolorder(z, c("id", time, "value"))
    id <- "id"
  }

  cname <- list(id = id,
                time = time,
                value = "value")

  z <- dts_init(z)
  setattr(z, "cname", cname)
  z
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tibbletime <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tibbletime") return(x)
  ts_tibbletime_dts(ts_dts(x))
}
