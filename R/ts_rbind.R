# x <- ts_xts(AirPassengers)
# x <- x[-3]
# ll <- ts_rbind(AirPassengers, AirPassengers)
# ll <- ts_rbind(mdeaths, AirPassengers)


#' @rdname ts_bind
#' @export
ts_rbind <- function(...){

  ll <- list(...)

  desired.class <- desired_class(ll)
  # cl <- vapply(ll, function(e) class(e)[1], "")

  # TODO: keep df only and ts/mts only as their classes
  ll.dts <- lapply(ll, ts_dts)
  llnames <- lapply(substitute(placeholderFunction(...))[-1], deparse)

  nc <- vapply(ll.dts, NCOL, 1L)

  if (length(nc) == 1){
    # rbind multiple series if a single object is given
    o1 <- ll.dts[[1]]
    ll.dts <- list()
    for (i in 1:NCOL(o1)){
      ll.dts[[i]] <- o1[, i]
    }
    llnames <- ts_varnames(o1)
  }

  if (length(unique(nc)) != 1){
    stop("number of columns differ: ", paste(unique(nc), collapse = ", "))
  }

  z <- ll.dts[[1]]

  time.name <- colnames(z)[1]

  time.class <- class(z[[1]])[1]

  ind0 <- ll.dts[[1]][[1]]

  for (i in 2:length(ll.dts)){
    indi <- ll.dts[[i]][[1]]
    dup <- indi %in% ind0
    if (any(dup)) {
      message("duplicate timestamps removed in: ", llnames[i])
    }

    z1 <- ll.dts[[i]][!dup]

    # make sure time.name is the same everywhere, i.e., as in the first obj
    if (colnames(z1)[1] != time.name){
      colnames(z1)[1] <- time.name
    }

    if (class(z1[[1]])[1] != time.class){
      if (time.class == "Date") as_fun <- as.Date
      if (time.class == "POSIXct") as_fun <- as.POSIXct
      z1[[1]] <- as_fun(z1[[1]])
    }
    z <- rbind(z,  rm_dts_class(z1))
    ind0 <- z[[1]]
  }

 
  coerce_to_(desired.class)(z)

}


