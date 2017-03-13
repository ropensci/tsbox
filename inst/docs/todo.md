# Stuff


- Ts Functions

  - [X] as_xts(), as_ts(), as_df()
  - [X] tsplot(), tsggplot(), theme_ts() (-> theme_tsbox)
  - [X] ts_colors() (-> tsbox_colors)
  
  - [X] tsdiff()
  - [X] tsseas()
  - [X] tsforecast()
  - [X] tsbind()
  - [X] tsrbind()

  - [ ] Rewrite existing generics / methods, using ts
  
    - [ ] tspcy()
    - [ ] tspc()
    - [ ] tsdiffy()
    - [ ] tstrend() -> tsloess
    - [ ] tsscale()
    
  - [ ] Cleaner documentation on ts Functions:
    - first argumetn is a single or multiple time series
    - additional arguments via dots, but help should be available for the methods as well. 
      Can we do this? Ideally, the pop up rstudio should show what tsloess is doing?
    - Overview of all available ts Funktions.
  
- Backend

  - [X] date_to_time, time_to_date: Rewrite carefully
  - [X] back and forth testing of the converters
  - [ ] remove zoo import, import xts
  - [ ] suggest ggplot

- Frequency etc

  - [ ] regularity check for xts
  - [ ] forcing xts into regularity
  - [ ] tools for frequency conversion

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

