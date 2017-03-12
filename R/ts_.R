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
#' @export
#' @examples
#' tsplot(
#'     tsbind(AirPassengers, mdeaths),
#'     tsforecast(tsbind(AirPassengers, mdeaths))
#' )
#' 
ts_ <- function(FUN, class = "ts", multiple = TRUE, suggested = NULL){

  all.classes <- c("ts", "mts", "data.frame", "data.table")
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
      z <- settsnames(z, tsn)
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
      z <- settsnames(z, tsn)
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
tsseas <- ts_(function(x, ...) seasonal::final(seasonal::seas(x, ...)),
              multiple = FALSE, suggested = "seasonal")

#' @export
#' @param n how many princial components should be extracted
#' @rdname ts_
tsprcomp <- ts_(function(x, n = 1, ...) {
  ts(predict(prcomp(x))[,1:n], start = start(x), frequency = frequency(x))
})











# This is how tspc could be written mauch simpler
# tspc <- ts_(function(x, ...){
#   if (NCOL(x) > 1){
#     return(tsapply(x, tspc))
#   }
#   100 * ((x / stats::lag(x, -1)) - 1)
# })

# tsdiff(as_xts(AirPassengers))



