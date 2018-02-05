
# to ---------------------------------------------------------------------------

ts_data.frame_dts <- function(x){
  as.data.frame(ts_data.table(x))
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts data.frame
ts_dts.data.frame <- function(x){
  ts_dts(as.data.table(x))
}


# main converter ---------------------------------------------------------------

# could be probably automated

#' @name ts_ts
#' @export
ts_data.frame <- function(x){
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "data.frame") return(x)
  ts_data.frame_dts(ts_dts(x))
}

#' @name ts_ts
#' @export
ts_df <- function(x){
  ts_data.frame(x)
}


