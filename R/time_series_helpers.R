
#' @export
ts_na_omit <- function(x){
  z <- ts_dts(x)
  coerce_to_(relevant_class(x))(z[!is.na(value)])
}

