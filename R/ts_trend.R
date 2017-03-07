
# Optimal Loess Parameter by AIC
# 
# Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing 
# parameter selection in nonparametric regression using an improved 
# Akaike Information Criterion. Journal of the Royal Statistical 
# Society B 60: 271–293.
# 
# http://www.stat.washington.edu/courses/stat527/s13/readings/j_royal_stat_soc_b1998.pdf
# 
# code inspiration from: https://gist.github.com/kylebgorman/6444612
#
# careful: the loess.as function from package fANCOVA is WRONG. It uses
#    aicc = log(sigma2) + 1 + 2 * (2 * (trace + 1)) / (n - trace - 2)
# which is not what the paper says. One 2 too much! The Author, wangx6@ccf.org, 
# said he will correct it. Let's see...
#
#' @export
ts_trend <- function (x, ...) UseMethod("ts_trend")



#' @export
#' @method ts_trend xts
ts_trend.xts <- function(x, degree = 2, span = NULL){

  if (is.null(span)){
    span <- loess_aic_span_optim(x = x, degree = degree)
    message("'span' automatically set to: ", span)
  }
  
  m <- loess(x ~ seq(x), span = span, degree = degree)

  # also compute standard errors
  pp <- predict(m, se = TRUE)

  z <- pp$fit

  # z <- cbind(mean = pp$fit, 
  #           lowerCI = -1.96 * pp$se.fit + pp$fit, 
  #           upperCI = 1.96 * pp$se.fit + pp$fit
  #           )

  reclass(z, x)
}


loess_aic_span_optim <- function(x, degree = 2){
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



#' @export
#' @method ts_trend ts
ts_trend.ts <- function(x, ...){
  as_ts(ts_trend(as_xts(x), ...))
}

