#' Pick Series (Experimental)
#'
#' Pick (and optionally rename) series from multiple time series.
#'  
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param ... names of the series to be picked (quoted or unquoted, for interactive use). If arguments are named, the series will be renamed.
#' @param .id character string, names of the series to be picked (for programming). If elements are named, the series will be renamed.
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' # Interactive use
#' ts_plot(ts_pick(
#'   EuStockMarkets, 
#'   `My Dax` = DAX, 
#'   `My Smi` = SMI
#' ))
#' 
#' # Programming use
#' to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
#' ts_pick(EuStockMarkets, .id = to.be.picked.and.renamed)
#' @export
ts_pick <- function(x, ..., .id = NULL) {
  stopifnot(ts_boxable(x))

  id <- NULL
  call.names <- unlist(lapply(substitute(placeholderFunction(...))[-1], deparse, 
                              width.cutoff = 500L))

  # prepare names of the series
  # unqote 
  call.names <- gsub("\"", "", call.names, fixed = TRUE)
  call.names <- gsub("\'", "", call.names, fixed = TRUE)

  if (is.null(names(call.names))) names(call.names) <- call.names
  names(call.names)[names(call.names) == ""] <- call.names[names(call.names) == ""]

  if (is.null(.id)){
    .id <- call.names
  }

  z <- combine_id_cols(ts_dts(x))
  setkey(z, id)
  z <- z[.id]
  z$id <- as.factor(z$id)
  levels(z$id) <- names(.id)[match(levels(z$id), .id)]
  z$id <- as.character(z$id)

  copy_class(z, x)
}




