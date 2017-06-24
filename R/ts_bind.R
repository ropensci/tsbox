
#' Bind any time series vertically or horizontally
#' 
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @examples
#'
#' ts_bind(ts_df(EuStockMarkets), AirPassengers)
#' ts_bind(EuStockMarkets, mdeaths)
#'
#' # labelling:
#' ts_bind(`International Airline Passengers` = ts_xts(AirPassengers), 
#'        `Deaths from Lung Diseases` = ldeaths)
#' 
#' ts_rbind(ts_df(mdeaths), AirPassengers)
#' ts_rbind(ts_xts(AirPassengers), mdeaths)
#' 
#' @export
ts_bind <- function(...){

  ll <- list(...)

  if (length(ll) == 1) return(ll[[1]])

  desired.class <- desired_class(ll)

  # currently we treat ts-only bind separately, if of same freq
  if (desired.class == "ts"){
    ll.dts <- ll
  } else {
    ll.dts <- lapply(ll, ts_dts)
  }

  lcnames <- lapply(ll.dts, colnames)
  is.single <- vapply(ll.dts, function(e) NCOL(e) == 1L, FALSE)

  if (any(is.single)){
    call.names <- lapply(substitute(placeholderFunction(...))[-1], deparse)
    relevant.names <- call.names 

    if (!is.null(names(call.names))){
      for (i in 1:length(call.names)){
        # only do this for single series
        if (is.single[i]){

          # TODO colnames seems silly here...
          if (names(call.names)[i] == ""){  
            # 3. prio: use variable names if nothing else is given
            if (is.null(colnames(ll.dts[[i]]))){
              relevant.names[[i]] <- call.names[[i]]
            } else {
              # 2. prio: use colnames if given
              cn <- colnames(ll.dts[[i]])

              stopifnot(length(cn) == 1)
              relevant.names[[i]] <- cn
            }
          } else {       
            # 1. prio: always use name for single series if given
            relevant.names[[i]] <- names(call.names)[i]
          }
        }
      }
      
    }
    lcnames[is.single] <- relevant.names[is.single]
  }

  # currently we treat ts-only bind separately, if of same freq
  if (desired.class == "ts"){
    z <- do.call("cbind", ll.dts)
    colnames(z) <- unlist(lcnames)
  } else {
    ll.dts <- lapply(ll, ts_dts)

    # rename var as in lcnames
    ll.dts <- Map(function(dt, vname) dt[, var := vname], dt = ll.dts, vname = unlist(lcnames))
    z <- rbindlist(ll.dts)
  }

  coerce_to_(desired.class)(z)

}
