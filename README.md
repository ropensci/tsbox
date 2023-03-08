
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsbox: Class-Agnostic Time Series in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci/tsbox/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/tsbox/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/tsbox/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/tsbox?branch=main)
<!-- badges: end -->

The R ecosystem knows a [vast
number](https://CRAN.R-project.org/view=TimeSeries) of time series
standards. Instead of creating the ultimate
[15th](https://xkcd.com/927/) time series class, tsbox provides a set of
tools that are **agnostic towards the existing standards**. The tools
also allow you to handle time series as plain data frames, thus making
it easy to deal with time series in a
[dplyr](https://CRAN.R-project.org/package=dplyr) or
[data.table](https://CRAN.R-project.org/package=data.table) workflow.

See [tsbox.help](https://docs.ropensci.org/tsbox/) for the full
documentation of the package.

To install the stable version from CRAN:

``` r
install.packages("tsbox")
```

To install the development version:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/tsbox")
install.packages("ropensci/tsbox", repos = "https://ropensci.r-universe.dev")
```

### Convert everything to everything

tsbox is built around a set of converters, which convert time series
stored as **ts**, **xts**, **data.frame**, **data.table**, **tibble**,
**zoo**, **zooreg**, **tsibble**, **tibbletime**, **timeSeries**,
**irts** or **tis** to each other:

``` r
library(tsbox)
x.ts <- ts_c(fdeaths, mdeaths)
x.xts <- ts_xts(x.ts)
x.df <- ts_df(x.xts)
x.dt <- ts_dt(x.df)
x.tbl <- ts_tbl(x.dt)
x.zoo <- ts_zoo(x.tbl)
x.zooreg <- ts_zoo(x.zoo)
x.tsibble <- ts_tsibble(x.zooreg)
x.tibbletime <- ts_tibbletime(x.tsibble)
x.timeSeries <- ts_timeSeries(x.tibbletime)
x.irts <- ts_irts(x.tibbletime)
x.tis <- ts_tis(x.irts)
all.equal(ts_ts(x.tis), x.ts)
#> [1] TRUE
```

### Use same functions for time series classes

Because this works reliably, it is easy to write functions that work for
all classes. So whether we want to **smooth**, **scale**,
**differentiate**, **chain**, **forecast**, **regularize** or
**seasonally adjust** a time series, we can use the same commands to
whatever time series class at hand:

``` r
ts_trend(x.ts)
ts_pc(x.xts)
ts_pcy(x.df)
ts_lag(x.dt)
```

### Time series of the world, unite!

A set of helper functions makes it easy to combine or align multiple
time series of all classes:

``` r
# collect time series as multiple time series
ts_c(ts_dt(EuStockMarkets), AirPassengers)
ts_c(EuStockMarkets, mdeaths)

# combine time series to a new, single time series
ts_bind(ts_dt(mdeaths), AirPassengers)
ts_bind(ts_xts(AirPassengers), ts_tbl(mdeaths))
```

### And plot just about everything

Plotting all kinds of classes and frequencies is as simple as it should
be. And we finally get a legend!

    ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[ ,'DAX'])))

![](https://raw.githubusercontent.com/ropensci/tsbox/master/vignettes/fig/myfig.png)

### Cheatsheet

<a href="https://www.cynkra.com/blog/2019-04-10-tsbox-01/tsbox-cheatsheet.pdf"><img style="max-width:50%;" src="https://www.cynkra.com/blog/2019-04-10-tsbox-01/tsbox-cheatsheet-small.jpg"></a>
