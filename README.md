Time Series Toolbox
===================

[![Build Status](https://travis-ci.org/christophsax/tsbox.svg?branch=master)](https://travis-ci.org/christophsax/tsbox)

A toolbox to deal with time series in R. Built around a set of converters, which
*reliably* convert time series stored as`ts`, `xts`, `data.frame` or
`data.table` to each other. Because this works, we can define a set of tools
that work *the same way* for each class. And, we can write a plot function that
*just works*!

To install:
```
devtools::install_github("christophsax/tsbox")
```

### Convert everything to everything

```
library(tsbox)
library(data.table)  # if you want to use the 'data.table' methods

x.ts <- tsbind(mdeaths, fdeaths) 
x.xts <- as_xts(x.ts)
x.df <- as_df(x.xts)
x.dt <- as_dt(x.df)
```

### Use same generic functions for ts, xts, data.frame or data.table

All functions start with `ts`, so you use it with auto complete.

```
tsscale(x.ts)
tsscale(x.xts)
tsscale(x.df)
tsscale(x.dt)

tstrend(x.ts)
tstrend(x.xts)
tstrend(x.df)
tstrend(x.dt)

tspc(x.ts)
tspc(x.xts)
tspc(x.df)
tspc(x.dt)

tspcy(x.ts)
tspcy(x.xts)
tspcy(x.df)
tspcy(x.dt)
```

### Bind any time series vertically or horizontally

```
tsbind(as_dt(EuStockMarkets), AirPassengers)
tsbind(EuStockMarkets, mdeaths)

tsrbind(as_dt(mdeaths), AirPassengers)
tsrbind(as_xts(AirPassengers), mdeaths)
```

### And plot just about everything

```
tsplot(tsscale(tsbind(discoveries, austres, AirPassengers)))      
```
![](https://github.com/christophsax/tsbox/raw/master/inst/docs/graph.jpg)


It uses [ggplot2](https://CRAN.R-project.org/package=ggplot2), so you can 
continue the usual way:

```
tsplot(tsscale(tsbind(discoveries, austres, AirPassengers))) + 
  theme_grey()

```


### License

*tsbox* is free and open source, licensed under GPL-3. 

