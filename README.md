R Time Series Toolbox
=====================

[![Build Status](https://travis-ci.org/christophsax/tsbox.svg?branch=master)](https://travis-ci.org/christophsax/tsbox)

*This is an early version, so expect major changes. Thanks for [feedback](mailto:christoph.sax@gmail.com)!*


tsbox provides tools that are *agnostic* towards time series concepts. 
The R ecosystem knows a [vast number](https://cran.r-project.org/web/views/TimeSeries.html) 
of time series standards. Instead of creating the ulitmate
[15th](https://xkcd.com/927/) time series class, tsbox provides a set of tools
that are agnostic towards the existing standards. The tools also allow you to
handle time series as plain data frames, thus making it easy to deal with time
series in a [dplyr](https://CRAN.R-project.org/package=dplyr) or
[data.table](https://CRAN.R-project.org/package=data.table) workflow.

tsbox is built around a set of converters, which reliably convert time series
stored as **ts**, **xts**, **data.frame**, **data.table** or  **tibble** to each
other. Because this works smoothly, we can define a set of tools that work
*identially* for each class. And, we can write a plot function that simply
works!

To install:
```r
devtools::install_github("christophsax/tsbox")
```

### Convert everything to everything

```r
library(tsbox)
library(data.table)  # if you want to use the 'data.table' methods
library(dplyr)       # if you want to use the 'tibble' methods

x.ts <- ts_c(mdeaths, fdeaths)
x.xts <- ts_xts(x.ts)
x.df <- ts_df(x.xts)
x.dt <- ts_dt(x.df)
x.tbl <- ts_tbl(x.dt)
```

### Use same functions for ts, xts, data.frame, data.table or tibble

All functions start with `ts`, so you use them with auto complete (press Tab).

```r
ts_scale(x.ts)  # normalization
ts_scale(x.xts)
ts_scale(x.df)
ts_scale(x.dt)
ts_scale(x.tbl)

ts_trend(x.ts)  # loess trend line
ts_pc(x.ts)
ts_pcy(x.ts)
ts_lag(x.ts)
ts_prcomp(ts_c(mdeaths, fdeaths))  # first principal component

# with external packages
ts_forecast(x.ts)  # ets forecast
ts_seas(x.ts)      # X-13 seasonal adjustment
```

### Bind any time series vertically or horizontally

```r
ts_c(ts_dt(EuStockMarkets), AirPassengers)
ts_c(EuStockMarkets, mdeaths)

ts_rbind(ts_dt(mdeaths), AirPassengers)
ts_rbind(ts_xts(AirPassengers), ts_tbl(mdeaths))
```

### And plot just about everything

```r
ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX'])))
```
![](https://github.com/christophsax/tsbox/raw/master/inst/docs/myfig.png)


There is also a version that uses [ggplot2](https://CRAN.R-project.org/package=ggplot2):

```r
ts_ggplot(ts_scale(ts_c(discoveries, austres, AirPassengers)))
```


### More expamples

#### Writing ts functions

The `ts_` function is a constructor function for tsbox time series functions.
Use it to wrap any function that works with time series. The defaults are set to
`ts`, so wrapping base functions for `ts` objects is as simple as:

```r
ts_diff <- ts_(diff)
```

Or a more complex example, which uses an external package:

```r
ts_forecast <- ts_(
  function(x, ...) {
    forecast::forecast(x, ...)$mean
  },
  multiple = FALSE, suggested = forecast
  )
```

Note that the `ts_` function deals with the conversion stuff, 'verctorizes' the
function so that it can be used with mulitple time series and also ask the user
to install the required packages.


#### Using tsbox in a dplyr / pipe workflow

```r
library(dplyr)
library(tsbox)

dta <- ts_tbl(ts_c(mdeaths, fdeaths))

dta %>%
  ts_c(lmdeaths = ts_lag(ts_select(dta, 'mdeaths'), -1)) %>%
  ts_predictlm(mdeaths ~ lmdeaths + fdeaths) %>%
  ts_plot()
```


### License

*tsbox* is free and open source, licensed under GPL-3.

