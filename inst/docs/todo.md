- [X] ts_compound() should start at 1
- [X] unify ts_lag and ts_shift?

- [C] ts_window(), extend regular series, fill w NA?
- [X] ts_align vs ts_union, bad naming, one should be enough, -> ts_intersect, 
      only keep common time stamps, no need to regularize, unless its ts
      - removed altogther, they are confusing and unneeded. 
      ts_window(template = ) is probably more useful.
      
- [ ] Avoid error when col order is wrong
- [ ] How useful are the date_ functions?
      for standard aggregation, we have now ts_frequency, 
      for nonstandard aggregation, e.g. hours to weeks, we need something like 
      lubridate anyway. Is there any usecase?

      date_shift should prob become time_shift, mostly used internally 
- [ ] ts_frequency with incomplete periods?


- Documentation

  - [ ] LONGTERM Documentation for auto generated ts_ functions.
        It would be nice if help is also available. 
        Can we do this? Ideally, the pop up rstudio should show what ts_seas is doing?

- Frequency Conversion

  - [ ] LONGTERM Conversion to higher frequency (needs tempdisagg update)

- Graph etc

  - [ ] LONGTERM legend in base plot: It would be to have a variable space 
        between entries, but not sure if possible
  - [ ] LONGTERM ts_iframe not sure if this belongs here, but it is just nice
        to create an iframe with a single command, as on www.dataseries.org


# Motivation

### People struggling with xts to ts conversion

http://stackoverflow.com/questions/14705783/coercing-xts-to-ts-without-specifying-start-and-end-date

http://stackoverflow.com/questions/35696119/coerce-xts-to-ts-in-r

http://stackoverflow.com/questions/32455478/force-xts-object-to-ts



### People struggling with data.frame to ts conversion

https://stackoverflow.com/questions/34664669/how-to-add-time-series-objects-ts-in-a-data-table-by-row



### People struggling with plots




### People struggling with frequency conversion

