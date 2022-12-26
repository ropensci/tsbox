register_class("data.frame")

# to ---------------------------------------------------------------------------

#' Convert to Class
#' @noRd
ts_data.frame_dts <- function(x) {
  as.data.frame(ts_data.table(x))
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts data.frame
ts_dts.data.frame <- function(x) {
  ts_dts(as.data.table(x))
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_data.frame <- function(x) {
  check_ts_boxable(x)
  if (relevant_class(x) == "data.frame") {
    return(x)
  }
  ts_data.frame_dts(ts_dts(x))
}

#' @name ts_ts
#' @export
ts_df <- function(x) {
  ts_data.frame(x)
}
