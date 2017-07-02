relevant_names <- function(call.names, list){
  stopifnot(inherits(call.names, "list"))


  # if (is.null(names(call.names))) {
  #   return(NULL)
  # }

  relevant.names <- call.names 

  for (i in 1:length(call.names)){
    # only do this for single series
    if (length(call.names[[i]]) == 1){
      if (is.null(names(call.names)) || names(call.names)[i] == ""){  

        # 3. prio: use variable names if nothing else is given
        if (ts_varnames(list[[i]])[1] %in% c("", ".unnamed")) {
          relevant.names[[i]] <- call.names[[i]]
        } else {
          # 2. prio: use colnames if given
          cn <- ts_varnames(list[[i]])

          # stopifnot(length(cn) == 1)
          relevant.names[[i]] <- cn
        }
      } else {       
        # 1. prio: always use name for single series if given
        relevant.names[[i]] <- names(call.names)[i]
      }
    }
  }
  relevant.names
}




#' Bind any time series vertically or horizontally
#' 
#' @param ... time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @examples
#'
#' ts_c(ts_df(EuStockMarkets), AirPassengers)
#' ts_c(EuStockMarkets, mdeaths)
#'
#' # labelling:
#' ts_c(`International Airline Passengers` = ts_xts(AirPassengers), 
#'        `Deaths from Lung Diseases` = ldeaths)
#' 
#' ts_c(a = ts_df(mdeaths), AirPassengers)
#' ts_rbind(ts_xts(AirPassengers), mdeaths)
#' ts_c(ts_dt(EuStockMarkets), AirPassengers)
#' ts_select(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX']), 'mdeaths')
#' 
#' @export
ts_c <- function(...){

  ll <- list(...)

  if (length(ll) == 1) return(ll[[1]])

  desired.class <- desired_class(ll)

  # currently we treat ts-only bind separately, if of same freq
  # if (desired.class == "ts"){
  #   ll.dts <- ll
  # } else {
    ll.dts <- lapply(ll, ts_dts, cname = ".unnamed")
  # }

  vnames <- lapply(ll.dts, ts_varnames)
  is.nameable <- vapply(ll.dts, function(e) ts_nvar(e) == 1, FALSE)

  call.names <- lapply(substitute(placeholderFunction(...))[-1], deparse)
  relevant.names <- relevant_names(call.names, list = ll.dts)
  vnames <- relevant.names   #[is.nameable]

  # ensure names are unique
  vnames <- relist(make.unique(unlist(vnames)), skeleton = vnames)

  # currently we treat ts-only bind separately, if of same freq
  # if (desired.class == "ts"){
  #   z <- do.call("cbind", ll.dts)
  #   colnames(z) <- vnames
  # } else {
    # rename var as in vnames
    ll.dts <- Map(function(dt, vname) ts_set_names(dt, vname),  dt = ll.dts, vname = vnames)
    ll.dts <- unify_class(ll.dts)

    # browser()
    z <- rbindlist(ll.dts)
  # }

  coerce_to_(desired.class)(z)

}


# ll <- ll.dts
unify_class <- function(ll){
  cl <- vapply(ll, function(e) class(e[[1]])[1], "")
  if (length(unique(cl)) > 1) {
    ll[cl == "Date"] <-  lapply(ll[cl == "Date"], function(e) change_class.data.table(e, colnames(e)[1], "as.POSIXct"))
  }
  ll
}






