- [X] ts_compound() should start at 1
- [X] unify ts_lag and ts_shift?
- [X] ts_align vs ts_union, bad naming, one should be enough, -> ts_intersect, 
      only keep common time stamps, no need to regularize, unless its ts
      - removed altogther, they are confusing and unneeded. 
      ts_span(template = ) is probably more useful.
- [X] time_shift should prob become time_shift, mostly used internally 
- [X] if time_shift works ts_lag will work with high freq as well
- [X] copy_class should be able to deal with length 1 series, so we can do
  ts_span(mdeaths, start = ts_end(mdeaths))
- [X] redesign dts
- [X] How to extract last values? use ts_span(AirPassengers, end = -1) 
- [X] regularization, shifting of regular, non standard series (EuStockmarkets)
- [X] ts_index base default
- [X] add proper class registry to specific files
- [X] unified and automated class testing: each class in registry should go through a bunch of tests.
- [X] as.POSIXct(idx, origin = "1970-01-01"), whats the correct origin? This 
      almost surely isnt. Perhaps use ISODate?
- [X] POSIXct does not merge well. Use rolling joins instead.
- [X] How useful are the date_ functions?
- [X] use bquote, rather than eval parse text (to_from_tsibble, but perhaps elsewhere)
- [X] ts_frequency with incomplete periods?
- [ ] use bquote everywhere

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

### People struggling with time series plots

### People struggling with frequency conversion

