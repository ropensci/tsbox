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
#'    `predict()`. (See examples)
#' @export
#' @examples
#' ts_(rowSums)(ts_c(mdeaths, fdeaths))
#' ts_plot(mean = ts_(rowMeans)(ts_c(mdeaths, fdeaths)), mdeaths, fdeaths)
#' ts_(function(x) predict(prcomp(x)))(ts_c(mdeaths, fdeaths))
#' ts_(function(x) predict(prcomp(x, scale = TRUE)))(ts_c(mdeaths, fdeaths))
ts_ <- function(f, class = "ts", vectorize = FALSE) {
  supported.classes <- c("ts", "mts", "xts", "data.frame", "data.table", "tbl", 
                         "dts")
  stopifnot(class %in% supported.classes)

  ts_to_class <- as.name(paste0("ts_", class))
  if (vectorize){
    z <- substitute(function(x, ...){
      fun <- function(x, ...){
        stopifnot(ts_boxable(x))
        z <- f(ts_to_class(x), ...)
        ts_reclass(z, x)
      }
      ts_apply(x, fun, ...)
    })
  } else {
    z <- substitute(function(x, ...){
      stopifnot(ts_boxable(x))
      z <- f(ts_to_class(x), ...)
      ts_reclass(z, x)
    })
  }

  f <- eval(z, parent.frame())
  attr(f, "srcref") <- NULL   # fix so prints correctly (from dtplyr)
  f
}




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



