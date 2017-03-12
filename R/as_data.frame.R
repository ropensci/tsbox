
#' @export
#' @rdname as_xts
as_data.frame <- function (x, ...) UseMethod("as_data.frame")


#' @export
#' @rdname as_xts
as_df <- function (x, ...) {
  as_data.frame(x, ...)
}

#' @import zoo xts
#' @rdname as_xts
#' @export
#' @method as_data.frame xts
as_data.frame.xts <- function(x, ...){
  # if (!melt) stop("not yet implemented")
  df <- zoo::fortify.zoo(zoo::as.zoo(x), melt = TRUE)
  colnames(df) <- c("time", "variable", "value")
  if (NCOL(x) == 1){
    df$variable <- NULL
  } else {
    if (is.factor(df$variable)){
      df$variable <- as.character(df$variable)
    }
  }
  if (any(class(df$time) %in% c("yearqtr", "yearmon"))){
    df$time <- zoo::as.Date.yearmon(df$time)
  }

  df
}

#' @export
#' @rdname as_xts
#' @method as_data.frame ts
as_data.frame.ts <- function(x, ...){
  as_data.frame(as_xts(x), ...)
}



#' @export
#' @rdname as_xts
#' @method as_data.frame data.frame
as_data.frame.data.frame <- function(x, ...){
  x
}


#' @export
#' @rdname as_xts
#' @method as_data.frame data.table
as_data.frame.data.table <- function(x, ...){
  as.data.frame(x)
}
