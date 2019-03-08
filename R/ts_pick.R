#' Pick Series (Experimental)
#'
#' Pick (and optionally rename) series from multiple time series.
#'
#' @inherit ts_dts
#' @param ... character string(s), names of the series to be picked, or integer, with positions.
#'   If arguments are named, the series will be renamed.
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' # Interactive use
#' \donttest{
#' ts_plot(ts_pick(
#'   EuStockMarkets,
#'   `My Dax` = "DAX",
#'   `My Smi` = "SMI"
#' ))
#' head(ts_pick(EuStockMarkets, c(1, 2)))
#' head(ts_pick(EuStockMarkets, `My Dax` = 'DAX', `My Smi` = 'SMI'))
#' }
#'
#' # Programming use
#' to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
#' head(ts_pick(EuStockMarkets, to.be.picked.and.renamed))
#' @export
ts_pick <- function(x, ...) {
  stopifnot(ts_boxable(x))

  id <- NULL
  call.names <- unlist(lapply(substitute(placeholderFunction(...))[-1], deparse,
                              width.cutoff = 500L))

  .id <- c(...)

  if (is.null(names(.id))) names(.id) <- .id
  names(.id)[names(.id) == ""] <- .id[names(.id) == ""]

  x.dts <- ts_dts(x)
  if (ncol(x.dts) == 2) return(x)  # do nothing with singel time series

  z <- combine_id_cols(x.dts)

  cname <- dts_cname(z)

  if (is.numeric(.id)) {
    names.id <- names(.id)
    base.id <- as.character(unname(.id))
    .id <- unique(z[[cname$id]])[.id]
    if (!identical(names.id, base.id)){
      .id <- setNames(.id, names.id)
    } else {
      .id <- setNames(.id, .id)
    }

  }

  missing.in.data <- !(.id %in% z[[cname$id]])
  if (any(missing.in.data)) {
    stop("values missing in data: ", paste(.id[missing.in.data], collapse = ", "), call. = FALSE)
  }

  setkeyv(z, cname$id)
  z <- z[.id]
  z[[cname$id]] <- as.factor(z[[cname$id]])
  levels(z[[cname$id]]) <- names(.id)[match(levels(z[[cname$id]]), .id)]
  z[[cname$id]] <- as.character(z[[cname$id]])

  copy_class(z, x)
}




