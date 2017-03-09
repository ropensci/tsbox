#' @export
#' @rdname as_xts
as_ts <- function (x, ...) UseMethod("as_ts")

#' @export
#' @rdname as_xts
#' @method as_ts xts
as_ts.xts <- function(x, ...) {

  p <- xts::periodicity(x)
  if (p$scale == "yearly"){
    ti <- as.POSIXlt(index(x))$year + 1900L
    f <- 1
    z <- ts(coredata(x), start = ti[1], frequency = f)
  } else if (p$scale == "monthly"){
    f = 12
    index(x) <- zoo::as.yearmon(index(x))
    z <- as.ts(as.zoo(x))
  } else if (p$scale == "quarterly"){
    f = 4
    index(x) <- zoo::as.yearqtr(index(x))
    z <- as.ts(as.zoo(x))
  } else if (p$scale == "daily"){
    ti <- date_to_time(x)

    f <- length(ti) /  (ti[length(ti)] - ti[1])
    z <- ts(coredata(x), start = ti[1], frequency = f)

  } else {
    stop("freq not yet implemented")
  }

  z


}



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









