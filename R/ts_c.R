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
        if (colnames(list[[i]])[1] %in% c("", ".unnamed")) {
          relevant.names[[i]] <- call.names[[i]]
        } else {
          # 2. prio: use colnames if given
          cn <- colnames(list[[i]])

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
#' a <- ts_c(EuStockMarkets, mdeaths)
#'
#' # labelling:
#' ts_c(`International Airline Passengers` = ts_xts(AirPassengers), 
#'        `Deaths from Lung Diseases` = ldeaths)
#' 
#' ts_c(a = mdeaths, AirPassengers)
#' ts_bind(ts_xts(AirPassengers), mdeaths)
#' ts_c(ts_dt(EuStockMarkets), AirPassengers)
#' ts_bind(ts_dt(mdeaths), AirPassengers)
#' 
#' @export
ts_c <- function(...){

  ll <- list(...)

  if (length(ll) == 1) return(ll[[1]])

  call.names <- unlist(lapply(substitute(placeholderFunction(...))[-1], deparse))
  # use name if specified in call
  call.names[names(call.names) != ""] <- names(call.names)[names(call.names) != ""]

  desired.class <- desired_class(ll)

  # special treatment for ts
  # - speed gain
  # - solves spacing issue for in series NAs
  if (desired.class == "ts"){
    no.cnames <- vapply(ll, function(e) length(colnames(e)) <= 1, TRUE)
    if (is.null(names(ll))) names(ll) <- call.names
    names(ll)[no.cnames] <- call.names[no.cnames]
    z <- do.call(cbind, ll)
    colnames(z) <- make.unique(colnames(z))
    return(z)
  }

  ll.dts <- lapply(ll, ts_dts)
  vnames <- lapply(ll.dts, colname_id)

  colname.id <- colname_id(ll.dts[[1]])

  # In case first element is unnamed series
  if (length(colname.id) == 0){
    colname.id <- "id"
  }

  # add names from call for single series
  is.unnamed <- vapply(ll.dts, function(e) ncol(e) == 2, FALSE)

  ll.dts[is.unnamed] <- Map(
    function(dt, id) {
      dt$id <- id
      setcolorder(dt, c(3, 1, 2))
      dt
    }, 
    dt = ll.dts[is.unnamed], 
    id = call.names[is.unnamed]
  )

  # TODO ensure uniqueness of ids!

  if (length(unique(lapply(ll.dts, colname_id))) > 1) {
    stop("id dimensions must be the same for all elements.")
  }
  
  ll.dts <- unify_time_class(ll.dts)

  z <- rbindlist(ll.dts)

  z <- try(coerce_to_(desired.class)(z))

  if (inherits(z, "try-error")){
    message("Cannot coerce output to class '", class, "', returning data.frame.")
    z <- ts_df(z)
  }
  z

}


# ll <- ll.dts
unify_time_class <- function(ll){
  cl <- vapply(ll, function(e) class(e[[2]])[1], "")
  if (length(unique(cl)) > 1) {
    ll[cl == "Date"] <-  lapply(ll[cl == "Date"], function(e) change_class.data.table(e, colnames(e)[2], "as.POSIXct"))
  }
  ll
}





# #' @export
# #' @name ts_ts
# ts_c.dts <- function(...){
#   ll <- list(...)
#   if (!inherits(ll[[1]], "dts")){
#     ll <- ll[[1]]
#   }
#   is.dts <- vapply(ll, function(e) inherits(e, "dts"), TRUE)
#   stopifnot(all(is.dts))  # TODO better message
#   z <- rbindlist(ll)
#   add_dts_class(z)
# }

