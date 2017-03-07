

# library(dataseries)
# sent <- ds("CCI.CCIIR", "xts")
# gdp <- ts_pc(ds("GDP.PBRTT.A.R", "xts"))
# ts_ggplot(ts_scale(ts_cbind(sent, gdp)))


#' @export
as_df <- function (x, ...) UseMethod("as_df")


#' @import zoo
#' @export
#' @method as_df xts
as_df.xts <- function(x, melt = TRUE){
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
#' @method as_df ts
as_df.ts <- function(x, ...){
  as_df(as.xts(x), ...)
}




#' @export
#' @method as_ts data.frame
as_ts.data.frame <- function(x, time.name = "time", variable.name = "variable", value.name = "value"){
  as_ts(as_xts(x, time.name = time.name, variable.name = variable.name, value.name = value.name))
}







# #' @export
# #' @method as_df xts
# as_df.ts <- function(x){
#   if (inherits(x, "mts")){
#     ll <- list()
#     for (i in seq(NCOL(x))){
#       ll[[i]] <- xts::as.xts(x[,i])
#     }
#     x.xts <- do.call(cbind, ll)
#     names(x.xts) <- colnames(x)
#   } else {
#     x.xts <- xts::as.xts(x)
#   }

#   df <- mytools:::fortify.xts(x.xts)
#   if (NCOL(x) == 1){
#     df$Series <- NULL
#   }
#   if (class(df$Index) %in% c("yearqtr", "yearmon")){
#     df$Index <- zoo::as.Date.yearmon(df$Index)
#   }
#   df
# }
