R Time Series Toolbox
=====================

[![Build Status](https://travis-ci.org/christophsax/tsbox.svg?branch=master)](https://travis-ci.org/christophsax/tsbox)

*This is an early version, so expect some changes. Thanks for [feedback](mailto:christoph.sax@gmail.com)!*

tsbox provides functions that are *agnostic* towards time series classes. 
The R ecosystem knows a [vast number](https://cran.r-project.org/web/views/TimeSeries.html) 
of time series standards. Rather than creating the ultimate
[15th](https://xkcd.com/927/) time series class, tsbox provides a set of tools
that are agnostic towards the existing standards. The tools also allow you to
handle time series as plain data frames, thus making it easy to deal with time
series in a [dplyr](https://CRAN.R-project.org/package=dplyr) or
[data.table](https://CRAN.R-project.org/package=data.table) workflow.

tsbox is built around a set of converters, which reliably convert time series
stored as **ts**, **xts**, **data.frame**, **data.table** or **tibble** to each
other. Because this works reliably and without user input, we can easily write
functions that work for all classes. So whether we want to smooth, scale,
differentiate or bind time series, you can use the same commands to whatever
time series class we have. And, most conveniently, we get a time series plot
function that *just works*!

**Update Version 0.0.9 (July 2, 17):** Major update, now using [data.table](https://CRAN.R-project.org/package=data.table) as a backend, instead of [xts](https://CRAN.R-project.org/package=xts).

To install:
```r
devtools::install_github("christophsax/tsbox")
```

### Convert everything to everything

```r
library(tsbox)

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

ts_trend(AirPassengers)  # loess trend line
ts_pc(x.ts)
ts_pcy(x.ts)
ts_lag(x.ts)

# with external packages
ts_forecast_mean(mdeaths)  # ets forecast
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
ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = ts_select(EuStockMarkets ,'DAX'))))
```
![](https://github.com/christophsax/tsbox/raw/master/inst/docs/myfig.png)


There is also a version that uses [ggplot2](https://CRAN.R-project.org/package=ggplot2):

```r
ts_ggplot(ts_scale(ts_c(discoveries, austres, AirPassengers)))
```


### More examples

#### Writing ts functions

The `ts_` function is a constructor function for tsbox time series functions.
Use it to wrap any function that works with time series. The defaults are set to
`ts`, so wrapping base functions for `ts` objects is as simple as:

```r
ts_(diff)(AirPassengers)
ts_(rowSums)(ts_c(mdeaths, fdeaths))
```

Or a more complex example, which uses a post processing function:

```r
ts_prcomp <- ts_(prcomp, predict, scale = TRUE)
ts_prcomp(ts_c(mdeaths, fdeaths))
```

Note that the `ts_` function deals with the conversion stuff, 'verctorizes' the
function so that it can be used with mulitple time series.


#### Using tsbox in a dplyr / pipe workflow

```r
library(dplyr)
library(tsbox)

dta <- ts_tbl(ts_c(mdeaths, fdeaths))

dta %>%
  ts_c(lmdeaths = ts_lag(ts_select(dta, 'mdeaths'), -1)) %>%
  ts_plot()
```


### List of Functions

This is an overview of all the functions in tsbox. Planned functions are (in
parentheses). If you would add something else, or suggest different naming or
conceptualization, please let me know.


#### Convert

    ts_ts
    ts_xts
    ts_df       # or ts_data.frame
    ts_dt       # or ts_data.table
    ts_tbl

#### Bind

    ts_c        # vertically
    ts_rbind    # horizontally

#### Filter and Select

    ts_window
    ts_align
    ts_select

#### Transform

    ts_scale    # normalization
    ts_trend    # loess trend line
    ts_pc
    ts_pcy
    ts_diff
    ts_lag
    (ts_seas)     # requires(seasonal)

#### Signal Extraction

    (ts_prcomp)   # principal component

#### Frequency Conversion

    (ts_to_frequency)               # requires(tempdisagg)

#### Forecast

    ts_forecast_mean              # requires(forecast)
    ts_forecast_auto.arima_mean   # requires(forecast)
    (ts_arima_predict)
    (ts_lm_predict)

#### Plot

    ts_plot
    ts_ggplot
    (ts_dygraph)    # requires(dygraph)
    (ts_iframe)     # requires(dygraph), returns iframe containing the data


### License

*tsbox* is free and open source, licensed under GPL-3.

