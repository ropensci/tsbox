
context("README.md")



test_that("examples from README.md work properly", {

  x.ts <- ts_c(mdeaths, fdeaths)
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)

  ts_df(ts_c(fdeaths, mdeaths))
  
  ts_scale(x.ts)  # normalization
  ts_scale(x.xts)
  ts_scale(x.df)
  ts_scale(x.dt)

  ts_trend(AirPassengers)  # loess trend line
  ts_pc(x.ts)
  ts_pcy(x.ts)
  ts_lag(x.ts)

  # with external packages
  ts_forecast_mean(mdeaths)  # ets forecast
  # ts_seas(x.ts)  # X-13 seasonal adjustment

  ts_c(ts_dt(EuStockMarkets), AirPassengers)
  ts_c(EuStockMarkets, mdeaths)

  ts_bind(ts_dt(mdeaths), AirPassengers)
  ts_bind(ts_xts(AirPassengers), mdeaths)


  # ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX'])))
  # ts_ggplot(ts_scale(ts_c(discoveries, austres, AirPassengers)))
  # ts_plot(ts_scale(ts_c(discoveries, austres, AirPassengers)))


  dta <- ts_df(ts_c(mdeaths, fdeaths))

  # dta %>%
  #   ts_c(lmdeaths = ts_lag(ts_select(dta, 'mdeaths'), -1)) %>%
  #   # ts_predictlm(mdeaths ~ lmdeaths + fdeaths) %>%
  #   ts_plot()


})


