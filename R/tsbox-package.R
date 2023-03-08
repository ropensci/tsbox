#' tsbox: Class-Agnostic Time Series
#'
#' The R ecosystem knows a vast number of time series classes: ts, xts, zoo,
#' tsibble, tibbletime, tis, or timeSeries. The plethora of standards causes
#' confusion.
#' As different packages rely on different classes, it is hard to use them in
#' the same analysis. tsbox provides a set of tools that make it easy to switch
#' between these classes. It also allows the user to treat time series as plain
#' data frames, facilitating the use with tools that assume rectangular data.
#'
#' The package is built around a set of functions that convert time series of
#' different classes to each other. They are frequency-agnostic, and allow the
#' user to combine multiple non-standard and irregular frequencies. Because
#' coercion works reliably, it is easy to write functions that work identically
#' for all classes. So whether we want to smooth, scale, differentiate,
#' chain-link, forecast, regularize or seasonally adjust a time series, we can
#' use the same tsbox-command for any time series classes.
#'
#' The best way to start is to check out the package
#' [website](https://docs.ropensci.org/tsbox/).
#'
#' In the *ropensci* classification, this package is *An improvement on other
#' implementations of similar algorithms in **R***. Many time series packages,
#' e.g., [zoo](https://CRAN.R-project.org/package=zoo) or
#' [tsibble](https://CRAN.R-project.org/package=tsibble) contain converter
#' functions from one class to another. They often convert from their class
#' to `ts` objects and back, but lack converters to other time series class.
#'
#' In most cases, tsbox transforms an object into an augmented `data.table`. And
#' uses the `data.table` infrastructure for efficient joining and reshaping. After
#' computation, it restores the original input class. This restoring feature is
#' was also used in the `xts::reclass()` function of the
#' [xts](https://CRAN.R-project.org/package=xts) package.
#'
#' @srrstats {G1.1} *Statistical Software should document whether the algorithm(s) it implements are:* - *The first implementation of a novel algorithm*; or - *The first implementation within **R** of an algorithm which has previously been implemented in other languages or contexts*; or - *An improvement on other implementations of similar algorithms in **R***.
#' @name tsbox-package
#' @aliases tsbox
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}
#' @keywords package
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'   roxygen2 is used for all documentation.
NULL
