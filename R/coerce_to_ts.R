#' @export
#' @rdname ts_xts
ts_ts <- function (x, ...) UseMethod("ts_ts")

#' @export
#' @rdname ts_xts
#' @method ts_ts xts
ts_ts.xts <- function(x, ...) {

  # if (NCOL(x) > 1) {
  #   zl <- list()
  #   for (i in 1:NCOL(x)){
  #     zl[[i]] <- ts_ts(na.omit(x[,i]))
  #   }

  #   z <- do.call(cbind, zl)
  #   z <- ts_set_names(z, ts_names(x))
  #   return(z)
  # }

  # check regularity

  # ud <- unique(round(diff(as.numeric(index(x)))))
  # if (length(ud) > 1) {
  #   stop("some dates in xts are not equally spaced. Equality must be enforced, but the tools to do so still need to be implemented.")
  # } 

  tsp <- Date_date_time_to_tsp(index(x))

  cdta <- coredata(x)
  if (NCOL(cdta) == 1) cdta <- as.numeric(cdta)

  z <- ts(cdta, start = tsp[1], frequency = tsp[3])
  z
}



# ts_ts.xts <- function(x, ...) {

#   p <- xts::periodicity(x)
#   if (p$scale == "yearly"){
#     ti <- as.POSIXlt(index(x))$year + 1900L
#     f <- 1
#     z <- ts(coredata(x), start = ti[1], frequency = f)
#   } else if (p$scale == "monthly"){
#     f = 12
#     index(x) <- zoo::as.yearmon(index(x))
#     z <- as.ts(as.zoo(x))
#   } else if (p$scale == "quarterly"){
#     f = 4
#     index(x) <- zoo::as.yearqtr(index(x))
#     z <- as.ts(as.zoo(x))
#   } else if (p$scale == "daily"){
#     ti <- date_to_time(x)

#     f <- length(ti) /  (ti[length(ti)] - ti[1])
#     z <- ts(coredata(x), start = ti[1], frequency = f)

#   } else {
#     stop("freq not yet implemented")
#   }

#   ts_set_names(z, ts_names(x))


# }




#' @export
#' @rdname ts_xts
#' @method ts_ts data.frame
ts_ts.data.frame <- function(x, ...){
  ts_ts(ts_xts(x, ...))
}


#' @export
#' @rdname ts_xts
#' @method ts_ts data.table
ts_ts.data.table <- function(x, ...){
  ts_ts(ts_xts(x, ...))
}

#' @export
#' @rdname ts_xts
#' @method ts_ts ts
ts_ts.ts <- function(x, ...){
  x
}









