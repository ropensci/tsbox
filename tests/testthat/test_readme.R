
context("README.md")



test_that("examples from README.md work properly", {

  x.ts <- tsbind(mdeaths, fdeaths)
  x.xts <- as_xts(x.ts)
  x.df <- as_df(x.xts)
  x.dt <- as_dt(x.df)

  tsscale(x.ts)  # normalization
  tsscale(x.xts)
  tsscale(x.df)
  tsscale(x.dt)

  tstrend(x.ts)  # loess trend line
  tspc(x.ts)
  tspcy(x.ts)
  tslag(x.ts)
  tsprcomp(tsbind(mdeaths, fdeaths))  # first principal component

  # with external packages
  tsforecast(x.ts)  # ets forecast
  # tsseas(x.ts)  # X-13 seasonal adjustment

  tsbind(as_dt(EuStockMarkets), AirPassengers)
  tsbind(EuStockMarkets, mdeaths)

  tsrbind(as_dt(mdeaths), AirPassengers)
  tsrbind(as_xts(AirPassengers), mdeaths)

  # tsplot(tsscale(tsbind(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX'])))
  # tsggplot(tsscale(tsbind(discoveries, austres, AirPassengers)))


  dta <- as_df(tsbind(mdeaths, fdeaths))

  dta %>%
    tsbind(lmdeaths = tslag(tsselect(dta, 'mdeaths'), -1)) %>%
    tspredictlm(mdeaths ~ lmdeaths + fdeaths) %>%
    tsplot()


})


