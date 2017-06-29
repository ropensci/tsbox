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

  - [ ] Rewrite existing generics / methods, using ts
  
    - [ ] ts_pcy()
    - [ ] ts_pc()
    - [ ] ts_diffy()
    - [ ] ts_trend() -> tsloess
    - [ ] ts_scale()

- Documentation

  - [ ] ts Functions:
    - first argument is a single or multiple time series
    - additional arguments via dots, but help should be available as well. 
      Can we do this? Ideally, the pop up rstudio should show what tsloess is doing?
    - Overview of all available ts Funktions.

- Type Conversion
  
  - [ ] rewrite ts_xts.ts, to include f = 2, f = 6, f = 0.1, f = 0.5, f = 0.05, f = 0.01
  - [ ] Ensure a Date to be regular, by filling NAs 

- Frequency Conversion

  - [ ] regularity check for xts
  - [ ] forcing xts into regularity
  - [ ] tools for frequency conversion

- Package

  - [ ] remove zoo import, import xts
  - [ ] suggest ggplot

- Graph etc

  - [ ] legend in base plot needs tweaking

- Beyond release

  - [ ] tsdygraphs
  - [ ] iframe option
  - [ ] 'theme' for base plot


# Motivation

### People struggling with xts to ts conversion

http://stackoverflow.com/questions/14705783/coercing-xts-to-ts-without-specifying-start-and-end-date

http://stackoverflow.com/questions/35696119/coerce-xts-to-ts-in-r

http://stackoverflow.com/questions/32455478/force-xts-object-to-ts


### People struggling with plots




### People struggling with frequency conversion

