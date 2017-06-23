

#' @export
#' @method ts_xts dts
ts_xts.dts <- function(x){
  stopifnot(requireNamespace("xts"))

  as.xts(spread_dts(x))
}


#' @export
#' @method dts xts
dts.xts <- function(x, var = NULL){
  stopifnot(requireNamespace("xts"))

  z <- as.data.table(x)
  setnames(z, "index", "time")

  if (NCOL(z) == 2){
    setnames(z, "V1", "value")
    single.var.name <- if (is.null(var)) deparse(substitute(x)) else var 
    z[, var := single.var.name]
  }

  gather_dts(z)
}
