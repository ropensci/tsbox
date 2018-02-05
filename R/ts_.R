load_suggested <- function(pkg){
  rns <- vapply(pkg, requireNamespace, TRUE)
  if (any(!rns)){
    pkgv <- dput(pkg[!rns])
    stop("Additional packages needed. To install, use:\n\n  install.packages(\"", pkgv, "\")", call. = FALSE)
  }
}


#' Universal Constructor for ts Functions
#' 
#' @param f function, to be made available to all time series classes
#' @param class class that the function uses as its first argument
#' @param vectorize should the function be vectorized? (not yet implemented)
#' @param postproc a function applied to the return value of `f()`. E.g. 
#'    `predict()`. (See examples)
#' @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param ... arguments passed to underlying functions.
#' @export
#' @examples
#' ts_(rowSums)(ts_c(mdeaths, fdeaths))
#' ts_plot(mean = ts_(rowMeans)(ts_c(mdeaths, fdeaths)), mdeaths, fdeaths)
#' ts_(prcomp, predict)(ts_c(mdeaths, fdeaths))
#' ts_(prcomp, predict, scale = TRUE)(ts_c(mdeaths, fdeaths))
ts_ <- function(f, postproc = function(x) x, class = "ts", vectorize = FALSE, ...) {
  supported.classes <- c("ts", "mts", "xts", "data.frame", "data.table", "tbl", "dts")
  stopifnot(class %in% supported.classes)
  z <- substitute(function(x, ...){
    stopifnot(ts_boxable(x))
    z <- f(coerce_to_(class)(x), ...)
    z <- postproc(z)
    ts_reclass(z, x)
  })
  f <- eval(z, parent.frame())
  f
}

#' @export
#' @importFrom stats window lag cycle lm prcomp start window "tsp<-"
#' @rdname ts_
ts_diff <- ts_(diff)


# #' @export
# #' @rdname ts_
# # ts_lag <- ts_(stats::lag)

# #' @export
# #' @rdname ts_
# ts_cycle <- ts_(stats::cycle, multi.series = FALSE)



# #' @export
# #' @rdname ts_
# ts_seas_final <- ts_(function(x, ...) {
#     seasonal::final(seasonal::seas(x, ...))
#   }, multi.series = FALSE, suggested.packages = "seasonal")

# #' @export
# #' @rdname ts_
# # @param n how many princial components should be extracted
# # @param scale should the data be scaled?
# ts_prcomp <- ts_(function(x, n = 1, scale = TRUE, ...) {
#   ts(predict(prcomp(x, scale = scale, ...))[,1:n], start = start(x), frequency = frequency(x))
# }, ensure.names = FALSE)



# # TODO: rework




# #' @export
# #' @rdname ts_
# ts_lm_resid <- ts_(function(x, formula, ...) {
#   m <- lm(formula = formula, data = as.data.frame(na.omit(x)), ...)
#   # message(paste(capture.output(summary(m)), collapse = "\n"))
#   reclass(resid(m), na.omit(x))
# }, specific.class = "xts", ensure.names = FALSE, suggested.packages = "xts")


# #' @export
# #' @rdname ts_
# ts_lm_predict <- ts_(function(x, formula, ...) {
#   m <- lm(formula = formula, data = as.data.frame(na.omit(x)), ...)
#   # message(paste(capture.output(summary(m)), collapse = "\n"))
#   reclass(predict(m), na.omit(x))
# }, specific.class = "xts", ensure.names = FALSE, suggested.packages = "xts")



# This is how ts_pc could be written mauch simpler
# ts_pc <- ts_(function(x, ...){
#   if (NCOL(x) > 1){
#     return(ts_apply(x, ts_pc))
#   }
#   100 * ((x / stats::lag(x, -1)) - 1)
# })

# ts_diff(ts_xts(AirPassengers))



