
#' Bind any time series vertically or horizontally
#' 
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @examples
#'
#' tsbind(as_df(EuStockMarkets), AirPassengers)
#' tsbind(EuStockMarkets, mdeaths)
#' 
#' tsrbind(as_df(mdeaths), AirPassengers)
#' tsrbind(as_xts(AirPassengers), mdeaths)
#' 
#' @export
tsbind <- function(...){


  ll <- list(...)

  desired.class <- desired_class(ll)

  ll.xts <- lapply(ll, as_xts)

  lcnames <- lapply(ll.xts, colnames)

  is.null.cnames <- vapply(lcnames, is.null, FALSE)

  if (any(is.null.cnames)){
    # good colnames
    list.with.names <- lapply(substitute(placeholderFunction(...))[-1], deparse)
    relevant.names <- list.with.names  # a template
    if (!is.null(names(list.with.names))){
      for (i in 1:length(list.with.names)){
        if (names(list.with.names)[i] == ""){  # no manual specification        
          relevant.names[[i]] <- list.with.names[[i]]
        } else {                               # manual specification
          relevant.names[[i]] <- names(list.with.names)[i]
        }
      }
    }
    lcnames[is.null.cnames] <- relevant.names[is.null.cnames]
  }

  
  z <- do.call("cbind", ll.xts)
  colnames(z) <- unlist(lcnames)
 
  as_(desired.class)(z)

}
