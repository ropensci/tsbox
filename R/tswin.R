#' @export
#' @rdname tspc
#' @export
tswin <- function (x, ...) UseMethod("tswin")

#' @export
#' @param start start date, string, Date or POSIXct
#' @param end end date, string, Date or POSIXct
#' @rdname tspc
#' @method tswin xts
tswin.xts <- function(x, start = NULL, end = NULL, ...){
  z <- x
  if (!is.null(start)){
    z <- z[paste0(as.POSIXct(start), "/")]
  }
  if (!is.null(end)){
    z <- z[paste0("/", as.POSIXct(end))]
  }
  z
}

#' @export
#' @rdname tspc
#' @method tswin ts
tswin.ts <- function(x, ...){
  as_ts(tswin(as_xts(x), ...))
}


#' @export
#' @rdname tspc
#' @method tswin data.frame
tswin.data.frame <- function(x, ...){
  as_data.frame(tswin(as_xts(x), ...))
}



#' @export
#' @rdname tspc
#' @method tswin data.table
tswin.data.table <- function(x, ...){
  as_data.table(tswin(as_xts(x), ...))
}


#' @export
#' @rdname tspc
#' @method tswin tbl
tswin.tbl <- function(x, ...){
  as_data.table(tswin(as_xts(x), ...))
}

