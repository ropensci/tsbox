
# Optimal Loess Parameter by AIC
#
# Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
# parameter selection in nonparametric regression using an improved
# Akaike Information Criterion. Journal of the Royal Statistical
# Society B 60: 271-293.
#
# http://www.stat.washington.edu/courses/stat527/s13/readings/j_royal_stat_soc_b1998.pdf
#
# code inspiration from: https://gist.github.com/kylebgorman/6444612


trend_core <- function(x, degree = 2, span = NULL) {
  value <- NULL

  z <- ts_dts(x)

  stopifnot(number_of_series(z) == 1)

  if (is.null(span)) {
    span <- loess_aic_span_optim(x = z[[2]], degree = degree)
    message(
      "'span' automatically set to ", formatC(span, 3),
      ". Set manually to change degree of smoothing"
    )
  }

  m <- loess(z[[2]] ~ seq(z[[2]]), span = span, degree = degree)

  # also compute standard errors
  pp <- predict(m)
  z[, value := pp]

  # z <- cbind(mean = pp$fit,
  #           lowerCI = -1.96 * pp$se.fit + pp$fit,
  #           upperCI = 1.96 * pp$se.fit + pp$fit
  #           )

  copy_ts_class(z, x)
}



loess_aic_span_optim <- function(x, degree = 2) {
  # aicc of loess obj
  aicc_loess <- function(loess.obj) {
    n <- loess.obj$n
    trace <- loess.obj$trace.hat
    sigma2 <- sum(resid(loess.obj) ^ 2) / (n - 1)
    log(sigma2) + 1 + 2 * (2 * (trace + 1)) / (n - trace - 2)
  }

  # choose loess obj that minimizes aicc
  objfun <- function(span) aicc_loess(loess(x ~ seq(x), span = span, degree = degree))
  span.optim <- optimize(objfun, c(0.05, 0.95))$minimum
}



# ts_trend(ts_c(mdeaths, fdeaths))




#' Loess Trend Estimation
#' 
#' Trend estimation that uses [stats::loess].
#' 
#' The Loess degree is chosen to minimize AIC, as described in:
#' 
#' Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
#' parameter selection in nonparametric regression using an improved
#' Akaike Information Criterion. Journal of the Royal Statistical
#' Society B 60: 271-293.
#'
#' @param x any time series object
#' @param ... arguments, passed to subfunction:
#' - `degree` degree of Loess smoothing
#' - `span` smoothing parameter, if `NULL`, an automated search performed (see Details)
#' @export
ts_trend <- ts_(trend_core, vectorize = TRUE)
