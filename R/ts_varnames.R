

ts_varnames <- function (x, ...) UseMethod("ts_varnames")

#' @export
#' @method ts_varnames ts
ts_varnames.ts <- function(x, ...){
  if (NCOL(x) > 1){
    colnames(x)
  } else {
    ""
  }
}


#' @export
#' @method ts_varnames xts
ts_varnames.xts <- function(x, ...){
  colnames(x)
}

#' @export
#' @method ts_varnames data.frame
ts_varnames.data.frame <- function(x, ...){
  unique(x$var)
}





ts_nvar <- function (x, ...) UseMethod("ts_nvar")

#' @export
#' @method ts_nvar ts
ts_nvar.ts <- function(x, ...){
  NCOL(x)
}


#' @export
#' @method ts_nvar xts
ts_nvar.xts <- function(x, ...){
  NCOL(x)
}

#' @export
#' @method ts_nvar data.frame
ts_nvar.data.frame <- function(x, ...){
  length(unique(x$var))
}
