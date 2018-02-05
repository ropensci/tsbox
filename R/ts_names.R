
# This does not work with arbitraty dims



# seem to be ok and usable elsewhere

#' Set and Extract Names of Time Series
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param value new names
#' @param ... additional arguments, passed to methods
#' @examples
#' 
#' x.ts <- ts_c(mdeaths, fdeaths) 
#' x.xts <- ts_xts(x.ts)
#' x.df <- ts_df(x.xts)
#' x.dt <- ts_dt(x.xts)
#'
#' ts_names(x.ts)
#' ts_names(x.xts)
#' ts_names(x.df)
#' ts_names(x.dt)
#' 
#' @export
ts_names <- function (x, ...) UseMethod("ts_names")

#' @export
#' @rdname ts_names
#' @method ts_names ts
ts_names.ts <- function(x, ...){
  if (NCOL(x) > 1) colnames(x) else NULL
}

#' @export
#' @rdname ts_names
#' @method ts_names xts
ts_names.xts <- function(x, ...){
  colnames(x)
}

#' @export
#' @rdname ts_names
#' @method ts_names data.frame
ts_names.data.frame <- function(x, ...){
  var.name = getOption("tsbox.var.name", "var")
  unique(x[[var.name]])
}

#' @export
#' @rdname ts_names
#' @method ts_names data.table
ts_names.data.table <- function(x, ...){
  var.name = getOption("tsbox.var.name", "var")
  unique(x[[var.name]])
}

#' @export
#' @rdname ts_names
#' @method ts_names tbl
ts_names.tbl <- function(x, ...){
  var.name = getOption("tsbox.var.name", "var")
  unique(x[[var.name]])
}



#' @export
#' @rdname ts_names
ts_set_names <- function (x, value) UseMethod("ts_set_names")

#' @export
#' @rdname ts_names
#' @method ts_set_names ts
ts_set_names.ts <- function(x, value){
  if (NCOL(x) > 1) `colnames<-`(x, value) else x 
}

#' @export
#' @rdname ts_names
#' @method ts_set_names xts
ts_set_names.xts <- function(x, value){
  `colnames<-`(x, value)
}

#' @export
#' @rdname ts_names
#' @method ts_set_names data.frame
ts_set_names.data.frame <- function(x, value){
  var.name = getOption("tsbox.var.name", "var")
  if (NCOL(x) == 3){
    z <- x
    z[[var.name]] <- set_unique(x[[var.name]], value)
  } else {
    z <- x
  }
  z
}


#' @export
#' @rdname ts_names
#' @method ts_set_names data.table
ts_set_names.data.table <- function(x, value){
  var.name = getOption("tsbox.var.name", "var")
  if (NCOL(x) == 3){
    z <- x
    z[[var.name]] <- set_unique(x[[var.name]], value)
  } else {
    z <- x
  }
  z
}


#' @export
#' @rdname ts_names
#' @method ts_set_names tbl
ts_set_names.tbl <- function(x, value){
  var.name = getOption("tsbox.var.name", "var")
  if (NCOL(x) == 3){
    z <- x
    z[[var.name]] <- set_unique(x[[var.name]], value)
  } else {
    z <- x
  }
  z
}




set_unique <- function(x, to){
  fr <- unique(x)
  for (i in seq(fr)){
    x[x==fr[i]] <- to[i]
  }
  x
}





