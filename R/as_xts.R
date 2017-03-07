#' @export
as_xts <- function (x, ...) UseMethod("as_xts")

#' @export
#' @method as_xts ts
as_xts.ts <- function(x){
  stopifnot(inherits(x, "ts"))
  m <- as.zoo(x)
  index(m) <- zoo::as.Date.yearmon(index(m))
  as.xts(m)
}


#' @export
#' @method as_xts xts
as_xts.xts <- function(x){
  x
}


#' @export
#' @method as_xts data.frame
as_xts.data.frame <- function(x, time.name = "time", variable.name = "variable", value.name = "value"){
  cnames <- colnames(x)
  stopifnot(time.name %in% cnames)

  as_xts_core <- function(x){
    xts(x = x[[time.name]], order.by = x[[time.name]])
  }
  if (variable.name %in% cnames){
    stopifnot(value.name %in% cnames)
    ll.xts <- lapply(split(x, x[[variable.name]]), as_xts_core)
    z <- do.call("cbind", ll.xts)
    colnames(z) <- names(ll.xts)
  } else {
    if (!value.name %in% cnames){
      if (NCOL(x) == 2){
        colnames(x)[!colnames(x) %in% time.name] <- "value"
      }
      stopifnot(value.name %in% cnames)
    }
    z <- as_xts_core(x)
    colnames(z) <- deparse(substitute(x))
  }
  
  z
}

#' @export
#' @method as_xts data.table
as_xts.data.table <- function(x, time.name = "time", variable.name = "variable", value.name = "value"){
  cnames <- colnames(x)
  stopifnot(time.name %in% cnames)

  stopifnot(requireNamespace("data.table"))

  as_xts_core <- function(x){
    setcolorder(x, c(time.name, value.name))
    as.xts(x)
  }
  if (variable.name %in% cnames){
    stopifnot(value.name %in% cnames)
    ll.xts <- lapply(split(x, x[[variable.name]]), as_xts_core)
    z <- do.call("cbind", ll.xts)
    colnames(z) <- names(ll.xts)
  } else {
    if (!value.name %in% cnames){
      if (NCOL(x) == 2){
        colnames(x)[!colnames(x) %in% time.name] <- "value"
      }
      stopifnot(value.name %in% cnames)
    }
    z <- as_xts_core(x)
    colnames(z) <- deparse(substitute(x))
  }
  
  z
}

