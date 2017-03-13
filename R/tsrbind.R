# x <- as_xts(AirPassengers)
# x <- x[-3]
# ll <- tsrbind(AirPassengers, AirPassengers)
# ll <- tsrbind(mdeaths, AirPassengers)


#' @rdname tsbind
#' @export
tsrbind <- function(...){

  ll <- list(...)

  desired.class <- desired_class(ll)
  # cl <- vapply(ll, function(e) class(e)[1], "")

  # TODO: keep df only and ts/mts only as their classes
  ll.xts <- lapply(ll, as_xts)
  llnames <- lapply(substitute(placeholderFunction(...))[-1], deparse)


  nc <- vapply(ll.xts, NCOL, 1L)

  if (length(nc) == 1){
    # rbind multiple series if a single object is given
    o1 <- ll.xts[[1]]
    ll.xts <- list()
    for (i in 1:NCOL(o1)){
      ll.xts[[i]] <- o1[, i]
    }
    llnames <- colnames(o1)
  }

  if (length(unique(nc)) != 1){
    stop("number of columns differ: ", paste(unique(nc), collapse = ", "))
  }

  z <- ll.xts[[1]]

  ind0 <- index(ll.xts[[1]])
  for (i in 2:length(ll.xts)){
    indi <- index(ll.xts[[i]])
    dup <- indi %in% ind0
    if (any(dup)) {
      message("duplicate timestamps removed in: ", llnames[i])
    }
    z <- rbind(z, ll.xts[[i]][!dup])
    ind0 <- index(z)
  }

 
  as_(desired.class)(z)

}


