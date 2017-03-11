
#' Set and Extract Names of Time Series
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param value new names
#' @param ... additional arguments, passed to methods
#' @examples
#' 
#' x.ts <- tsbind(mdeaths, fdeaths) 
#' x.xts <- as_xts(x.ts)
#' x.df <- as_df(x.xts)
#'
#' tsnames(x.ts)
#' tsnames(x.xts)
#' tsnames(x.df)
#' 
#' \dontrun{
#' library(data.table)  # if you want to use the 'data.table' methods
#' x.dt <- as_dt(x.df)
#' tsnames(x.dt)
#' }
#' 
#' @export
tsnames <- function (x, ...) UseMethod("tsnames")

#' @export
#' @rdname tsnames
#' @method tsnames ts
tsnames.ts <- function(x, ...){
  if (NCOL(x) > 1) colnames(x) else NULL
}

#' @export
#' @rdname tsnames
#' @method tsnames xts
tsnames.xts <- function(x, ...){
  colnames(x)
}

#' @export
#' @rdname tsnames
#' @method tsnames data.frame
tsnames.data.frame <- function(x, ...){
  unique(x$variable)
}


#' @export
#' @rdname tsnames
#' @method tsnames data.table
tsnames.data.table <- function(x, ...){
  unique(x$variable)
}


#' @export
settsnames <- function (x, value) UseMethod("settsnames")

#' @export
#' @rdname tsnames
#' @method settsnames ts
settsnames.ts <- function(x, value){
  if (NCOL(x) > 1) `colnames<-`(x, value) else x 
}

#' @export
#' @rdname tsnames
#' @method settsnames xts
settsnames.xts <- function(x, value){
  `colnames<-`(x, value)
}

#' @export
#' @rdname tsnames
#' @method settsnames data.frame
settsnames.data.frame <- function(x, value){
  if (NCOL(x) == 3){
    z <- set_unique(x$variable, value)
  } else {
    z <- x
  }
  z
}

#' @export
#' @rdname tsnames
#' @method settsnames data.table
settsnames.data.table <- function(x, value){
  if (NCOL(x) == 3){
    x$variable <- set_unique(x$variable, value)
  } 
  x
}



set_unique <- function(x, to){
  fr <- unique(x)
  for (i in seq(fr)){
    x[x==fr[i]] <- to[i]
  }
  x
}





