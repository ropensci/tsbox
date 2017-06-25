load_suggested_packages <- function(pkg){
  rns <- vapply(pkg, requireNamespace, TRUE)
  if (any(!rns)){
    pkgv <- dput(pkg[!rns])
    stop("Additional packages needed. To install, use:\n\n  install.packages(\"", pkgv, "\")", call. = FALSE)
  }
}



#' universal constructor for ts functions
#' 
#' @param FUN function, to be made available to all time series classes
#' @param class class that the function uses as its first argument
#' @param multi.series can the function handle multiple series. If set to false, the 
#'   wrapper will loop through each series.
#' @param suggested.packages packages that are required for the functionality.
#' @param ... arguments passed to underlying functions.
#' @param x s time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param ensure.names logical, whether the series in the output should have the same names as the input
#' @export
#' @examples
#' ts_plot(
#'     ts_bind(AirPassengers, mdeaths),
#'     ts_forecast(ts_bind(AirPassengers, mdeaths))
#' )
#' 
ts_ <- function(FUN, used.class = "ts", multi.series = TRUE, suggested.packages = NULL, ensure.names = TRUE){

  all.classes <- c("ts", "xts", "data.frame", "data.table", "tbl", "dts")
  stopifnot(used.class %in% all.classes)

  # if the function can handle multiple time series
  if (ensure.names){
    if (multi.series){
      z <- substitute(function(x, ...){
        load_suggested_packages(suggested.packages)
        stopifnot(class(x) %in% all.classes)
        z <- FUN(coerce_to_(used.class)(x), ...)
        z <- coerce_to_(relevant_class(x))(z)
        z <- ts_set_names(z, ts_names(x))
        z
      })
    } else {
      z <- substitute(function(x, ...){
        load_suggested_packages(suggested.packages)
        stopifnot(class(x) %in% all.classes)
        z <- ts_apply(coerce_to_(used.class)(x), FUN, ...)
        z <- coerce_to_(relevant_class(x))(z)
        z <- ts_set_names(z, ts_names(x))
        z
      })
    }  
  } else{
    if (multi.series){
      z <- substitute(function(x, ...){
        load_suggested_packages(suggested.packages)
        stopifnot(class(x) %in% all.classes)
        z <- FUN(coerce_to_(used.class)(x), ...)
        z <- coerce_to_(relevant_class(x))(z)
        z
      })
    } else {
       z <- substitute(function(x, ...){
        load_suggested_packages(suggested.packages)
        stopifnot(class(x) %in% all.classes)
        z <- ts_apply(coerce_to_(used.class)(x), FUN, ...)
        z <- coerce_to_(relevant_class(x))(z)
        z
      })
    }  
  }

  f <- eval(z, parent.frame())
  f
}

#' @export
#' @importFrom stats window lag cycle lm prcomp start
#' @rdname ts_
ts_diff <- ts_(diff)


#' @export
#' @rdname ts_
ts_lag <- ts_(stats::lag)

#' @export
#' @rdname ts_
ts_cycle <- ts_(stats::cycle, multi.series = FALSE)

#' @export
#' @rdname ts_
ts_forecast_mean <- ts_(function(x, ...) forecast::forecast(x, ...)$mean, 
                  multi = FALSE, suggested.packages = "forecast")

#' @export
#' @rdname ts_
ts_forecast_auto.arima_mean  <- ts_(
  function(x, confint = FALSE, ...) {
    forecast::forecast(forecast::auto.arima(x), ...)$mean
  }, multi.series = FALSE, suggested.packages = "forecast")

#' @export
#' @rdname ts_
ts_seas_final <- ts_(function(x, ...) {
    seasonal::final(seasonal::seas(x, ...))
  }, multi.series = FALSE, suggested.packages = "seasonal")

#' @export
#' @rdname ts_
# @param n how many princial components should be extracted
# @param scale should the data be scaled?
ts_prcomp <- ts_(function(x, n = 1, scale = TRUE, ...) {
  ts(predict(prcomp(x, scale = scale, ...))[,1:n], start = start(x), frequency = frequency(x))
}, ensure.names = FALSE)



# # TODO: rework


#' @export
#' @rdname ts_
# ts_scale <- ts_(function(x, ...){
#   z <- scale.default(unclass(x), ...)
#   xts::reclass(z, x)
# }, class = "xts")




# #' @export
# #' @rdname ts_
# ts_lm_resid <- ts_(function(x, formula, ...) {
#   m <- lm(formula = formula, data = as.data.frame(na.omit(x)), ...)
#   # message(paste(capture.output(summary(m)), collapse = "\n"))
#   reclass(resid(m), na.omit(x))
# }, used.class = "xts", ensure.names = FALSE, suggested.packages = "xts")


# #' @export
# #' @rdname ts_
# ts_lm_predict <- ts_(function(x, formula, ...) {
#   m <- lm(formula = formula, data = as.data.frame(na.omit(x)), ...)
#   # message(paste(capture.output(summary(m)), collapse = "\n"))
#   reclass(predict(m), na.omit(x))
# }, used.class = "xts", ensure.names = FALSE, suggested.packages = "xts")



# This is how ts_pc could be written mauch simpler
# ts_pc <- ts_(function(x, ...){
#   if (NCOL(x) > 1){
#     return(ts_apply(x, ts_pc))
#   }
#   100 * ((x / stats::lag(x, -1)) - 1)
# })

# ts_diff(ts_xts(AirPassengers))



