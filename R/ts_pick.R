#' Pick Series (Experimental)
#'
#' Pick (and optionally rename) series from multiple time series.
#'  
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param ... character string(s), names of the series to be picked. If arguments are named, the series will be renamed.
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' # Interactive use
#' ts_plot(ts_pick(
#'   EuStockMarkets, 
#'   `My Dax` = "DAX", 
#'   `My Smi` = "SMI"
#' ))
#' 
#' # Programming use
#' to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
#' ts_pick(EuStockMarkets, to.be.picked.and.renamed)
#' @export
ts_pick <- function(x, ...) {
  stopifnot(ts_boxable(x))

  id <- NULL
  call.names <- unlist(lapply(substitute(placeholderFunction(...))[-1], deparse, 
                              width.cutoff = 500L))

  .id <- c(...)
  if (is.null(names(.id))) names(.id) <- .id
  names(.id)[names(.id) == ""] <- .id[names(.id) == ""]

  z <- combine_id_cols(ts_dts(x))
  setkey(z, id)
  z <- z[.id]
  z$id <- as.factor(z$id)
  levels(z$id) <- names(.id)[match(levels(z$id), .id)]
  z$id <- as.character(z$id)

  copy_class(z, x)
}




