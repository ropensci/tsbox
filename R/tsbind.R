
#' Bind any time series vertically or horizontally
#' 
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @examples
#'
#' tsbind(as_df(EuStockMarkets), AirPassengers)
#' tsbind(EuStockMarkets, mdeaths)
#'
#' # labelling:
#' tsbind(`International Airline Passengers` = as_xts(AirPassengers), 
#'        `Deaths from Lung Diseases` = ldeaths)
#' 
#' tsrbind(as_df(mdeaths), AirPassengers)
#' tsrbind(as_xts(AirPassengers), mdeaths)
#' 
#' @export
tsbind <- function(...){

  ll <- list(...)

  if (length(ll) == 1) return(ll[[1]])
  desired.class <- desired_class(ll)
  ll.xts <- lapply(ll, as_xts)

  lcnames <- lapply(ll.xts, colnames)
  is.single <- vapply(ll.xts, function(e) NCOL(e) == 1L, FALSE)

  if (any(is.single)){
    call.names <- lapply(substitute(placeholderFunction(...))[-1], deparse)
    relevant.names <- call.names 
    if (!is.null(names(call.names))){
      for (i in 1:length(call.names)){
        if (names(call.names)[i] == ""){  
          # 3. prio: use variable names if nothing else is given
          if (is.null(colnames(ll.xts[[i]]))){
            relevant.names[[i]] <- call.names[[i]]
          } else {
            # 2. prio: use colnames if given
            cn <- colnames(ll.xts[[i]])
            stopifnot(length(cn) == 1)
            relevant.names[[i]] <- cn
          }
        } else {       
          # 1. prio: always use name for single series if given
          relevant.names[[i]] <- names(call.names)[i]
        }
      }
    }
    lcnames[is.single] <- relevant.names[is.single]
  }

  z <- do.call("cbind", ll.xts)
  colnames(z) <- unlist(lcnames)
 
  as_(desired.class)(z)

}
