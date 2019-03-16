#' tsbox: Class-Agnostic Time Series
#'
#' The R ecosystem knows a vast number of time series classes: ts, xts, zoo,
#' tsibble, tibbletime or timeSeries. The plethora of standards causes confusion.
#' As different packages rely on different classes, it is hard to use them in the
#' same analysis. tsbox provides a set of tools that make it easy to switch between
#' these classes. It also allows the user to treat time series as plain data
#' frames, facilitating the use with tools that assume rectangular data.
#'
#' The package is built around a set of functions that convert time series of
#' different classes to each other. They are frequency-agnostic, and allow the user
#' to combine multiple non-standard and irregular frequencies. Because coercion
#' works reliably, it is easy to write functions that work identically for all
#' classes. So whether we want to smooth, scale, differentiate, chain-link,
#' forecast, regularize or seasonally adjust a time series, we can use the same
#' tsbox-command for any time series class.
#'
#' The best way to start is to check out the package [website](https://www.tsbox.help).
#' @name tsbox-package
#' @aliases tsbox
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}
#' @keywords package
NULL

