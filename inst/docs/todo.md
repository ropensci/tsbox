<!-- 
library(tsbox)
ts_plot(AirPassengers, title = "Airline passengers", 
       subtitle = "The classic Box & Jenkins airline data")
ts_plot(total = ldeaths, female = fdeaths, male = mdeaths)

ts_plot(ts_c(sunspot.month, sunspot.year, lynx))
ts_plot(ts_scale(ts_c(airmiles, co2, JohnsonJohnson, discoveries)))
ts_plot(EuStockMarkets)
ts_plot(sunspot.month, sunspot.year, lynx)
ts_plot(ts_scale(ts_c(Nile, nottem, USAccDeaths)))
## Not run: 
ts_ggplot(AirPassengers)
ts_ggplot(total = ldeaths, female = fdeaths, male = mdeaths)

ts_ggplot(ts_c(sunspot.month, sunspot.year, lynx))
ts_ggplot(ts_scale(ts_c(airmiles, co2, JohnsonJohnson, discoveries)))
ts_ggplot(EuStockMarkets)
ts_ggplot(sunspot.month, sunspot.year, lynx)
ts_ggplot(ts_scale(ts_c(Nile, nottem, USAccDeaths)))

library(Quandl)
ts_ggplot(ts_df(Quandl::Quandl("FRED/GDPMC1")))

library(dataseries)
dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
ts_ggplot(ts_scale(ts_window(ts_c(`GDP Growth` = ts_pc(dta[, 'GDP.PBRTT.A.R']), 
                            `Consumer Sentiment Index` = dta[, 'CCI.CCIIR']), 
                     start = "1995-01-01")))


ts_df(Quandl("FRED/GDPMC1"))



x <- Quandl("FRED/GDPMC1")

ts_tbl(ts_c(sdfs = x, sdf = x)) %>% 
  ts_plot()

ts_tbl(Quandl::Quandl("FRED/GDPMC1"))

 -->

# Regularization

- Test for regularity
  - perhaps return reason for irregularity
- ts_reg() to force regularity
  - interpolation
  - alignment
  ...



# Stuff

- Ts Functions

  - [X] ts_xts(), ts_ts(), ts_df()
  - [X] ts_plot(), ts_ggplot(), theme_ts() (-> theme_tsbox)
  - [X] ts_colors() (-> tsbox_colors)
  
  - [X] ts_diff()
  - [X] ts_seas()
  - [X] ts_forecast()
  - [X] ts_c()
  - [X] ts_rbind()
  - [X] Rewrite existing generics / methods, using ts
  
    - [X] ts_pcy()
    - [X] ts_pc()
    - [X] ts_diffy()
    - [X] ts_trend() -> tsloess
    - [X] ts_scale()

- Documentation

  - [ ] ts Functions:
    - first argument is a single or multiple time series
    - additional arguments via dots, but help should be available as well. 
      Can we do this? Ideally, the pop up rstudio should show what tsloess is doing?
    - Overview of all available ts Funktions.

- Type Conversion
  
  - [ ] rewrite ts_xts.ts, to include f = 2, f = 6, f = 0.1, f = 0.5, f = 0.05, f = 0.01
  - [X] Ensure a Date to be regular, by filling NAs 

- Frequency Conversion

  - [X] regularity check for xts
  - [X] forcing xts into regularity
  - [X] tools for frequency conversion

- Package

  - [X] remove zoo import, import xts
  - [X] suggest ggplot

- Graph etc

  - [ ] legend in base plot needs tweaking

- Beyond release

  - [X] tsdygraphs
  - [ ] iframe option
  - [ ] 'theme' for base plot


# Motivation

### People struggling with xts to ts conversion

http://stackoverflow.com/questions/14705783/coercing-xts-to-ts-without-specifying-start-and-end-date

http://stackoverflow.com/questions/35696119/coerce-xts-to-ts-in-r

http://stackoverflow.com/questions/32455478/force-xts-object-to-ts



### People struggling with data.frame to ts conversion

https://stackoverflow.com/questions/34664669/how-to-add-time-series-objects-ts-in-a-data-table-by-row



### People struggling with plots




### People struggling with frequency conversion

