Time Series Toolbox
===================

[![Build Status](https://travis-ci.org/christophsax/tsbox.svg?branch=master)](https://travis-ci.org/christophsax/tsbox)

*This is a very early version, so expect major changes. Thanks for [feedback](mailto:christoph.sax@gmail.com)!*

A toolbox to deal with time series in R. Built around a set of converters, which
*reliably* convert time series stored as **ts**, **xts**, **data.frame**,
**data.table** or  **tibble** to each other. Because it works, we can define a
set of tools that work *identially* for each class. And, we can use a plot
function that *just works*!

To install:
```r
devtools::install_github("christophsax/tsbox")
```

### Convert everything to everything

```r
library(tsbox)
library(data.table)  # if you want to use the 'data.table' methods
library(dplyr)       # if you want to use the 'tibble' methods

x.ts <- ts_cbind(mdeaths, fdeaths)
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
ts_prcomp(ts_cbind(mdeaths, fdeaths))  # first principal component

# with external packages
ts_forecast(x.ts)  # ets forecast
ts_seas(x.ts)      # X-13 seasonal adjustment
```

### Bind any time series vertically or horizontally

```r
ts_cbind(ts_dt(EuStockMarkets), AirPassengers)
ts_cbind(EuStockMarkets, mdeaths)

ts_rbind(ts_dt(mdeaths), AirPassengers)
ts_rbind(ts_xts(AirPassengers), ts_tbl(mdeaths))
```

### And plot just about everything

```r
ts_plot(ts_scale(ts_cbind(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX'])))
```
![](https://github.com/christophsax/tsbox/raw/master/inst/docs/myfig.png)


There is also a version that uses [ggplot2](https://CRAN.R-project.org/package=ggplot2):

```r
ts_ggplot(ts_scale(ts_cbind(discoveries, austres, AirPassengers)))
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

dta <- ts_tbl(ts_cbind(mdeaths, fdeaths))

dta %>%
  ts_cbind(lmdeaths = ts_lag(ts_select(dta, 'mdeaths'), -1)) %>%
  ts_predictlm(mdeaths ~ lmdeaths + fdeaths) %>%
  ts_plot()
```


### License

*tsbox* is free and open source, licensed under GPL-3.

