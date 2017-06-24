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
    llnames <- colnames(o1)
  }

  if (length(unique(nc)) != 1){
    stop("number of columns differ: ", paste(unique(nc), collapse = ", "))
  }

  z <- ll.dts[[1]]

  ind0 <- ll.dts[[1]][, time]


  for (i in 2:length(ll.dts)){
    indi <- ll.dts[[i]][, time]
    dup <- indi %in% ind0
    if (any(dup)) {
      message("duplicate timestamps removed in: ", llnames[i])
    }
    z <- rbind(z, ll.dts[[i]][!dup])
    ind0 <- z[, time]
  }

 
  coerce_to_(desired.class)(z)

}


