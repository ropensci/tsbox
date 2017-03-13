#' @export
#' @rdname as_xts
as_ts <- function (x, ...) UseMethod("as_ts")

#' @export
#' @rdname as_xts
#' @method as_ts xts
as_ts.xts <- function(x, ...) {

  # if (NCOL(x) > 1) {
  #   zl <- list()
  #   for (i in 1:NCOL(x)){
  #     zl[[i]] <- as_ts(na.omit(x[,i]))
  #   }

  #   z <- do.call(cbind, zl)
  #   z <- settsnames(z, tsnames(x))
  #   return(z)
  # }

  # check regularity
  
  # ud <- unique(round(diff(as.numeric(index(x)))))
  # if (length(ud) > 1) {
  #   stop("some dates in xts are not equally spaced. Equality must be enforced, but the tools to do so still need to be implemented.")
  # } 

  tsp <- Date_POSIXct_to_tsp(index(x))
  z <- ts(coredata(x), start = tsp[1], frequency = tsp[3])

}



# as_ts.xts <- function(x, ...) {

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

#   settsnames(z, tsnames(x))


# }




#' @export
#' @rdname as_xts
#' @method as_ts data.frame
as_ts.data.frame <- function(x, time.name = "time", variable.name = "variable", value.name = "value", ...){
  as_ts(as_xts(x, time.name = time.name, variable.name = variable.name, value.name = value.name))
}


#' @export
#' @rdname as_xts
#' @method as_ts data.table
as_ts.data.table <- function(x, time.name = "time", variable.name = "variable", value.name = "value", ...){
  as_ts(as_xts(x, time.name = time.name, variable.name = variable.name, value.name = value.name))
}

#' @export
#' @rdname as_xts
#' @method as_ts ts
as_ts.ts <- function(x, ...){
  x
}









