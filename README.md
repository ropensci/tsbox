tsbox: Class-Agnostic Time Series in R
======================================

[![Build Status](https://travis-ci.org/christophsax/tsbox.svg?branch=master)](https://travis-ci.org/christophsax/tsbox)

The R ecosystem knows a [vast number](https://cran.r-project.org/web/views/TimeSeries.html) 
of time series standards. Instead of creating the ultimate
[15th](https://xkcd.com/927/) time series class, tsbox provides a set of tools
that are **agnostic towards the existing standards**. The tools also allow you to
handle time series as plain data frames, thus making it easy to deal with time
series in a [dplyr](https://CRAN.R-project.org/package=dplyr) or
[data.table](https://CRAN.R-project.org/package=data.table) workflow.

tsbox is built around a set of converters, which convert time series
stored as **ts**, **xts**, **data.frame**, **data.table** or **tibble** to each
other. Because this works reliably and without user input, we can easily write
functions that work for all classes. So whether we want to smooth, scale, differentiate, chain, forecast, regularize or seasonally adjust a time series, we can use the same commands to whatever
time series class at hand. And, most conveniently, we get a time series plot
function that works for all classes and frequencies.

To install:
```r
devtools::install_github("christophsax/tsbox")
```

### Convert everything to everything

tsbox can convert time series stored as **ts**, **xts**, **data.frame**, **data.table** or **tibble** to each other:

```r
library(tsbox)

x.ts <- ts_c(mdeaths, fdeaths)
x.xts <- ts_xts(x.ts)
x.df <- ts_df(x.xts)
x.dt <- ts_dt(x.df)
x.tbl <- ts_tbl(x.dt)
```

### Use same functions for ts, xts, data.frame, data.table or tibble

All functions start with `ts`, so you use them with auto complete (press Tab). These function work with any *ts-boxable* time series, ts, xts, data.frame, data.table or tibble, and **return the class of its input**.

```r
ts_scale(x.ts)           # normalization
ts_scale(x.xts)
ts_scale(x.df)
ts_scale(x.dt)
ts_scale(x.tbl)

ts_trend(AirPassengers)  # loess trend line
ts_pc(x.ts)
ts_pcy(x.ts)
ts_lag(x.ts)

# with external packages
ts_forecast(mdeaths)     # ets forecast
```

For a list of available functions, see below.


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
![](https://github.com/christophsax/tsbox/raw/master/inst/docs/myfig.png)


There is also a version that uses [ggplot2](https://CRAN.R-project.org/package=ggplot2):

```r
ts_ggplot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[ ,'DAX'])))
```


## More examples


### Time Series in Data Frames

Multiple time series will be stored as a 'long' data frame (`data.frame`,
`data.table` or `tibble`):

```r
ts_df(ts_c(fdeaths, mdeaths))

#          id       time value
# 1   fdeaths 1974-01-01   901
# 2   fdeaths 1974-02-01   689
# 3   fdeaths 1974-03-01   827
# 4   fdeaths 1974-04-01   677
# 5   fdeaths 1974-05-01   522
# ...
# 140 mdeaths 1979-08-01   975
# 141 mdeaths 1979-09-01   940
# 142 mdeaths 1979-10-01  1081
# 143 mdeaths 1979-11-01  1294
# 144 mdeaths 1979-12-01  1341
```

The time stamp, `time`, indicates the beginning of a period. tsbox requires the
columns in a data frame to follow either the order:

1. **id** column(s)
2. **time** column
3. **value** column

**or** the time and the value column to be explicitly named as `time` and `value`.
If explicit names are used, the column order will be ignored.

Note that **multiple id columns** with arbitrary names are allowed.


### Writing ts functions

The `ts_` function is a constructor function for tsbox time series functions.
Use it to wrap any function that works with time series. The default is set to
R base `"ts"` class, so wrapping functions for `"ts"` time series (or vectors or matrices) is as simple as:

```r
ts_(diff)(AirPassengers)
ts_(rowSums)(ts_c(mdeaths, fdeaths))
```

Or a more complex example, which uses a post processing function:

```r
ts_prcomp <- ts_(function(x) predict(prcomp(x, scale = TRUE)))
ts_prcomp(ts_c(mdeaths, fdeaths))
```

And some functions from external packages:

```r
ts_dygraphs <- ts_(dygraphs::dygraph, class = "xts")
ts_forecast <- ts_(function(x) forecast::forecast(x)$mean, vectorize = TRUE)
ts_seas <- ts_(function(x) seasonal::final(seasonal::seas(x)), vectorize = TRUE)

ts_dygraphs(ts_c(mdeaths, EuStockMarkets))
ts_forecast(ts_c(mdeaths, fdeaths))
ts_seas(ts_c(mdeaths, fdeaths))
```

Note that the `ts_` function deals with the conversion stuff, 'vectorizes' the
function so that it can be used with multiple time series.


### Using tsbox in a dplyr / pipe workflow

tsbox works well with tibbles and with the pipe, so it can be nicely integrated into a dplyr workflow:

```r
library(dplyr)
library(tsbox)

ts_tbl(ts_c(mdeaths, fdeaths)) %>% 
  ts_seas() %>% 
  ts_plot()
```

 
## Available Functions

This is an overview of the functions available in tsbox. If you would add something else, please let me [know](mailto:christoph.sax@gmail.com).


### Convert

|   Name   |               What it does              |
|----------|-----------------------------------------|
| `ts_ts`  | convert any time series to `ts`         |
| `ts_xts` | convert any time series to `xts`        |
| `ts_df`  | convert any time series to `data.frame` |
| `ts_dt`  | convert any time series to `data.table` |
| `ts_tbl` | convert any time series to `tibble`     |


### Combine

|    Name    |                   What it does                   |
|------------|--------------------------------------------------|
| `ts_c`     | collect time series as multiple time series      |
| `ts_bind`  | bin time series to a new, single time series     |
| `ts_chain` | chain time series, using percentage change rates |


### Align

|     Name    |                     What it does                    |
|-------------|-----------------------------------------------------|
| `ts_window` | filter time series for a time range.                |
| `ts_align`  | aligning span and frequencies (regular series only) |
| `ts_union`  | aligning time stamps (regular and irregular)        |


### Transform

|    Name    |                What it does               |
|------------|-------------------------------------------|
| `ts_scale` | normalization (center and scale)          |
| `ts_trend` | loess trend line                          |
| `ts_pc`    | percentage change rate                    |
| `ts_pcy`   | percentage change rate (to previous year) |
| `ts_diff`  | first difference                          |
| `ts_diffy` | first difference (to previous year)       |
| `ts_lag`   | lag operator                              |


### Reshape

|    Name   |                       What it does                       |
|-----------|----------------------------------------------------------|
| `ts_wide` | convert multiple time series to a wide data frame        |
| `ts_long` | convert wide data frame to a long, ts-boxable data frame |


### Frequency Handling

|      Name      |                  What it does                  |
|----------------|------------------------------------------------|
| `ts_frequency` | convert to lower frequency                     |
| `ts_regular`   | regularize: make implicit `NA` values explicit |
    

### Plot

|     Name    |                  What it does                  |
|-------------|------------------------------------------------|
| `ts_plot`   | a fast plotting function, with no dependencies |
| `ts_ggplot` | same syntax as `ts_plot`, returning a `ggplot` |
    

### Included examples of user defined ts_ functions

|     Name     |                    What it does                   |
|--------------|---------------------------------------------------|
| `ts_`        | universal constructor function for tsbox function |
| `ts_prcomp`  | principal components of multiple time series      |
| `ts_forcast` | automated forecasts (requires forecast)           |
| `ts_dygraph` | interactive graphs (requires dygraphs)            |
| `ts_seas`    | X-13 seasonal adjustment (requires seasonal)      |


## License

*tsbox* is free and open source, licensed under GPL-3.

*Thanks for [feedback](mailto:christoph.sax@gmail.com)!*

