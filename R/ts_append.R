#' Append values to a time series
#' 
#' Add a one or more elements of a vector to the end of a time series.
#' This is an S3 implementation such that appending works for a ts, xts, zoo, 
#' data.frame and data.table base. 
#' 
#' @export
#' @author Matthias Bannert
#' @examples 
#' ts1 <- ts(1:20,start = c(2000,2),frequency = 4)
#' ts1_df <- ts_df(ts1)
#' ts1_dt <- ts_data.table(ts1)
#' # Different append S3 methods
#' ts_append(ts1,30)
#' ts_append(ts1_df,c(30,40))
#' ts_append(ts1_dt,100)
ts_append <- function (x, ...) UseMethod("ts_append")

#' @export
#' @name ts_append
#' @method ts_append ts
ts_append.ts <- function(series,v){
  ts(c(series,v),start = start(series),
     frequency = frequency(series))
}


#' @export
#' @name ts_append
#' @method ts_append data.frame
ts_append.data.frame <- function(series,v){
  series <- ts_ts(series)
  series <- ts(c(series,v),start = start(series),
     frequency = frequency(series))
  ts_df(series)
}


#' @export
#' @name ts_append
#' @method ts_append xts
ts_append.xts <- function(series,v){
  series <- ts_ts(series)
  series <- ts(c(series,v),start = start(series),
     frequency = frequency(series))
  ts_xts(series)
}


#' @export
#' @name ts_append
#' @method ts_append zoo
ts_append.zoo <- function(series,v){
  series <- ts_ts(series)
  series <- ts(c(series,v),start = start(series),
     frequency = frequency(series))
  ts_zoo(series)
}


#' @export
#' @name ts_append
#' @method ts_append data.table
ts_append.data.table <- function(series,v){
  series <- ts_ts(series)
  series <- ts(c(series,v),start = start(series),
     frequency = frequency(series))
  ts_data.table(series)
}

