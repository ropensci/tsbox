

# library(dataseries)
# sent <- ds("CCI.CCIIR", "xts")
# gdp <- ts_pc(ds("GDP.PBRTT.A.R", "xts"))
# ts_ggplot(ts_scale(ts_cbind(sent, gdp)))



#' @export
as_data.frame <- function (x, ...) UseMethod("as_data.frame")


#' @export
as_df <- as_data.frame


#' @import zoo
#' @export
#' @method as_data.frame xts
as_data.frame.xts <- function(x, melt = TRUE){
  if (!melt) stop("not yet implemented")

  df <- zoo::fortify.zoo(zoo::as.zoo(x), melt = TRUE)
  colnames(df) <- c("time", "variable", "value")
  if (NCOL(x) == 1){
    df$variable <- NULL
  }
  if (any(class(df$time) %in% c("yearqtr", "yearmon"))){
    df$time <- zoo::as.Date.yearmon(df$time)
  }

  df
}

#' @export
#' @method as_data.frame ts
as_data.frame.ts <- function(x, ...){
  as_data.frame(as_xts(x), ...)
}


#' @export
#' @method as_data.frame data.frame
as_data.frame.data.frame <- function(x, ...){
  x
}

