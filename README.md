tsbox: Class-Agnostic Time Series in R
======================================

[![Build Status](https://travis-ci.org/christophsax/tsbox.svg?branch=master)](https://travis-ci.org/christophsax/tsbox)
[![codecov](https://codecov.io/github/christophsax/tsbox/branch/master/graphs/badge.svg)](https://codecov.io/github/christophsax/tsbox) 

The R ecosystem knows a [vast
number](https://cran.r-project.org/web/views/TimeSeries.html)  of time series
standards. Instead of creating the ultimate [15th](https://xkcd.com/927/) time
series class, tsbox provides a set of tools that are **agnostic towards the
existing standards**. The tools also allow you to handle time series as plain
data frames, thus making it easy to deal with time series in a
[dplyr](https://CRAN.R-project.org/package=dplyr) or
[data.table](https://CRAN.R-project.org/package=data.table) workflow.

tsbox is built around a set of converters, which convert time series stored as
**ts**, **xts**, **data.frame**, **data.table** or **tibble** to each other.

To install:
```r
devtools::install_github("christophsax/tsbox")
```

### Convert everything to everything

tsbox can convert time series stored as **ts**, **xts**, **data.frame**,
**data.table** or **tibble** to each other:

```r
library(tsbox)

x.ts <- ts_c(mdeaths, fdeaths)
x.xts <- ts_xts(x.ts)
x.df <- ts_df(x.xts)
x.dt <- ts_dt(x.df)
x.tbl <- ts_tbl(x.dt)
```

### Use same functions for ts, xts, data.frame, data.table or tibble

Because this works reliably and without user input, we can easily write
functions that work for all classes. So whether we want to smooth, scale,
differentiate, chain, forecast, regularize or seasonally adjust a time series,
we can use the same commands to whatever time series class at hand. And, most
conveniently, we get a time series plot function that works for all classes and
frequencies.

```r
ts_trend(x.ts) 
ts_pc(x.xts)
ts_pcy(x.df)
ts_lag(x.dt)
```


### Combine multiple time series

A set of helper functions makes it easy to combine or align multiple time
series, even if their classes are different:

```r
# collect time series as multiple time series
ts_c(ts_dt(EuStockMarkets), AirPassengers)
ts_c(EuStockMarkets, mdeaths)

# combine time series to a new, single time series
ts_bind(ts_dt(mdeaths), AirPassengers)
ts_bind(ts_xts(AirPassengers), ts_tbl(mdeaths))
```

### And plot just about everything

Because all of this works smoothly, plotting all kinds of classes and
frequencies is as simple as it should be. And it even has a legend!

```
ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[ ,'DAX'])))
```
![](https://raw.githubusercontent.com/christophsax/tsbox/master/vignettes/fig/myfig.png)


There is also a version that uses [ggplot2](https://CRAN.R-project.org/package=ggplot2):

```r
ts_ggplot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[ ,'DAX'])))
```


## License

tsbox is free and open source, licensed under GPL-3.

*Thanks for [feedback](mailto:christoph.sax@gmail.com)!*

