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
#' @param multiple can the function handle multiple series. If set to false, the 
#'   wrapper will loop through each series.
#' @param suggested packages that are required for the functionality.
#' @param ... arguments passed to underlying functions.
#' @param x s time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param ensure.names logical, whether the series in the output should have the same names as the input
#' @export
#' @examples
#' tsplot(
#'     tsbind(AirPassengers, mdeaths),
#'     tsforecast(tsbind(AirPassengers, mdeaths))
#' )
#' 
ts_ <- function(FUN, class = "ts", multiple = TRUE, suggested = NULL, ensure.names = TRUE){

  all.classes <- c("ts", "xts", "data.frame", "data.table")
  stopifnot(class %in% all.classes)

  # if the function can handle multiple time series
  if (multiple){
    z <- function(x, ...){
      load_suggested_packages(suggested)
      stopifnot(class %in% all.classes)
      desired.class <- relevant_class(x)
      tsn <- tsnames(x)
      z <- FUN(as_(class)(x), ...)
      z <- as_(desired.class)(z)
      if (ensure.names) z <- settsnames(z, tsn)
      z
    } 
  } else {
     z <- function(x, ...){
      load_suggested_packages(suggested)
      stopifnot(class %in% all.classes)
      desired.class <- relevant_class(x)
      tsn <- tsnames(x)
      z <- tsapply(as_(class)(x), FUN, ...)
      z <- as_(desired.class)(z)
      if (ensure.names) z <- settsnames(z, tsn)
      z
    } 
  } 
  return(z)
}

#' @export
#' @importFrom stats window lag cycle
#' @rdname ts_
tsdiff <- ts_(diff)

#' @export
#' @rdname ts_
tswindow <- ts_(stats::window)

#' @export
#' @rdname ts_
tslag <- ts_(stats::lag)

#' @export
#' @rdname ts_
tscycle <- ts_(stats::cycle)

#' @export
#' @rdname ts_
tsforecast <- ts_(function(x, ...) forecast::forecast(x, ...)$mean, 
                  multiple = FALSE, suggested = "forecast")

#' @export
#' @rdname ts_
tsforecast.auto.arima  <- ts_(
  function(x, ...) {
    forecast::forecast(forecast::auto.arima(x), ...)$mean
  }, multiple = FALSE, suggested = "forecast")

#' @export
#' @rdname ts_
tsseas <- ts_(function(x, ...) seasonal::final(seasonal::seas(x, ...)),
              multiple = FALSE, suggested = "seasonal")

#' @export
#' @rdname ts_
# @param n how many princial components should be extracted
# @param scale should the data be scaled?
tsprcomp <- ts_(function(x, ...) {
  ts(predict(prcomp(x, scale = TRUE, ...))[,1:n], start = start(x), frequency = frequency(x))
}, ensure.names = FALSE)




#' @export
#' @rdname ts_
tsresidlm <- ts_(function(x, formula, ...) {
  m <- lm(formula = formula, data = as.data.frame(na.omit(x)), ...)
  # message(paste(capture.output(summary(m)), collapse = "\n"))
  reclass(resid(m), na.omit(x))
}, class = "xts", ensure.names = FALSE)


#' @export
#' @rdname ts_
tspredictlm <- ts_(function(x, formula, ...) {
  m <- lm(formula = formula, data = as.data.frame(na.omit(x)), ...)
  # message(paste(capture.output(summary(m)), collapse = "\n"))
  reclass(predict(m), na.omit(x))
}, class = "xts", ensure.names = FALSE)




# This is how tspc could be written mauch simpler
# tspc <- ts_(function(x, ...){
#   if (NCOL(x) > 1){
#     return(tsapply(x, tspc))
#   }
#   100 * ((x / stats::lag(x, -1)) - 1)
# })

# tsdiff(as_xts(AirPassengers))



