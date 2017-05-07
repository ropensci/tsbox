
context("README.md")



test_that("examples from README.md work properly", {

  x.ts <- ts_bind(mdeaths, fdeaths)
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)

  ts_scale(x.ts)  # normalization
  ts_scale(x.xts)
  ts_scale(x.df)
  ts_scale(x.dt)

  ts_trend(x.ts)  # loess trend line
  ts_pc(x.ts)
  ts_pcy(x.ts)
  ts_lag(x.ts)
  ts_prcomp(ts_bind(mdeaths, fdeaths))  # first principal component

  # with external packages
  ts_forecast(x.ts)  # ets forecast
  # ts_seas(x.ts)  # X-13 seasonal adjustment

  ts_bind(ts_dt(EuStockMarkets), AirPassengers)
  ts_bind(EuStockMarkets, mdeaths)

  ts_rbind(ts_dt(mdeaths), AirPassengers)
  ts_rbind(ts_xts(AirPassengers), mdeaths)

  # ts_plot(ts_scale(ts_bind(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX'])))
  # ts_ggplot(ts_scale(ts_bind(discoveries, austres, AirPassengers)))


  dta <- ts_df(ts_bind(mdeaths, fdeaths))

  dta %>%
    ts_bind(lmdeaths = ts_lag(ts_select(dta, 'mdeaths'), -1)) %>%
    ts_predictlm(mdeaths ~ lmdeaths + fdeaths) %>%
    ts_plot()


})


