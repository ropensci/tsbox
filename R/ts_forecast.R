
#' @export
ts_forecast_auto.arima_mean  <- function(x, xreg = NULL, h = 2, ...){
  x0 <- x
  x <- ts_na_omit(x)
  rx <- ts_range(x)
  x <- ts_ts(x)

  if (!is.null(xreg)){
    xreg <- ts_ts(xreg)
    xreg <- ts_na_omit(xreg)
    rxreg <- ts_range(xreg)
    if (rxreg[1] > rx[1]){
      message("'start(xreg)' > 'start(x)'. Data before ", rxreg[1]," is discarded.")
      x <- ts_window(x, start = rxreg[1])
    }

    # probably need to do this using "ts" objects, so I can have sinlge value
    xreg.fct <- window(xreg, start = tsp(x)[2] + 1 / frequency(x))
    xreg <- ts_window(xreg, start = rx[1], end = rx[2])

    z <- forecast::forecast(forecast::auto.arima(x = x, xreg = xreg), 
                       xreg = xreg.fct)$mean

  } else {
    z <- forecast::forecast(forecast::auto.arima(x = x))$mean
  }

  ts_reclass(z, x0)

}

#' @export
ts_forecast_mean  <- function(xL, h = 2, ...){
  x0 <- x
  x <- ts_na_omit(x)
  z <- forecast::forecast(x)$mean
  ts_reclass(z, x0)
}
