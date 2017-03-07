# x <- as_xts(AirPassengers)
# x <- x[-3]
# ll <- ts_cbind(x, AirPassengers)

#' @export
ts_cbind <- function(...){

  # good colnames
  list.with.names <- lapply(substitute(placeholderFunction(...))[-1], deparse)
  relevant.names <- list.with.names  # a template
  if (!is.null(names(list.with.names))){
    for (i in 1:length(list.with.names)){
      if (names(list.with.names)[i] == ""){  # no manual specification        
        relevant.names[[i]] <- list.with.names[[i]]
      } else {                               # manual specification
        relevant.names[[i]] <- names(list.with.names)[i]
      }
    }
  }

  cnames <- unlist(relevant.names)
  
  ll <- list(...)
  # cl <- vapply(ll, function(e) class(e)[1], "")

  # TODO: keep df only and ts/mts only as their classes

  z <- do.call("cbind", lapply(ll, as_xts))
  colnames(z) <- cnames
  z

}
