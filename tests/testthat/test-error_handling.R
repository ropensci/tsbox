library(dplyr)

#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*

irreg1 <- data.frame(
  time = as.POSIXct(c(
    "2000-01-01", "2001-02-01", "2005-03-01", "2007-03-03", "2007-03-05",
    "2007-03-09", "2007-05-03", "2007-09-03"
  )),
  value = 1:8
)


test_that("errors work as expected", {
  # in order of search for 'stop0(''

  expect_error(
    ts_dts(lm),
    "object is of non-ts-boxable class"
  )

  expect_error(
    ts_lag(irreg1[1, ]),
    "need at least two observations for frequency detection"
  )

  expect_error(
    time_shift(irreg1$time, by = 1),
    "'by' cannot be integer when used with irregular sequence"
  )

  expect_error(
    ts_span(mdeaths, start = 1979, end = 1975),
    "'start' cannot be at or after 'end'"
  )
  expect_error(
    ts_span(mdeaths, start = 1979, end = 1975, extend = TRUE),
    "'start' cannot be at or after 'end'"
  )

  a <- ts_tbl(ts_c(mdeaths, fdeaths))
  b <- rename(a, id2 = id)
  expect_error(
    a %ts+% b,
    "\\[id\\] columns are not identical"
  )
  expect_error(
    ts_bind(a, b),
    "\\[id\\] columns are not identical"
  )

  x <- ts_tbl(mdeaths)
  x[2,1] <- NA
  expect_error(
    ts_dts(x),
   "\\[time\\] column contains missing values"
  )

  expect_error(
    ts_regular(irreg1),
    "series has no regular pattern"
  )

  expect_error(
    date_time_to_tsp(as.Date(c("2000-01-10", "2001-02-10", "2005-03-10"))),
   "some dates are not equally spaced"
  )

  expect_error(
    date_time_to_tsp(as.Date(c("2000-01-10", "2001-02-10", "2001-03-10"))),
   "sequence is not regular"
  )

  expect_error(
    date_time_to_tsp(as.Date(c("2001-01-10", "2001-02-10", "2001-03-10"))),
   "time column must be specified as the first date"
  )


  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    tidyr::nest(data = c(time, value))
  expect_error(
    ts_dts(x),
   "'x' contains list columns"
  )

  expect_error(
    ts_dts(bind_rows(ts_tbl(ts_c(mdeaths, fdeaths)), ts_tbl(ts_c(mdeaths, fdeaths)))),
   "object contains series with duplicated information"
  )

  expect_error(
    ts_dts(bind_rows(ts_tbl(mdeaths), ts_tbl(mdeaths))),
   "series contains duplicated values"
  )

  x <- ts_tbl(fdeaths)
  x$value <- as.character(x$value)
  expect_error(
    ts_dts(x),
   "is not numeric"
  )

  x <- ts_dts(fdeaths)
  x$time <- as.character(x$time)
  expect_error(
    tsbox:::guess_tattr(x),
   "\\[time\\] column is not of class 'Date' or 'POSIXct'"
  )

  x <- rename(ts_tbl(fdeaths), time2 = time)
  x$time2 <- "AAA"
  expect_error(
    ts_ts(x),
   "no \\[time\\] column detected"
  )

  x <- rename(ts_tbl(fdeaths), value2 = value)
  x$value2 <- "AAA"
  expect_error(
    ts_ts(x),
   "no \\[value\\] column detected"
  )

  expect_error(
    ts_ts(irreg1),
    "series has no regular pattern"
  )

  expect_error(
    load_suggested("pkg.that.does.not.exist"),
    "Additional packages needed"
  )

  expect_error(
    ts_(rowSums, vectorize = TRUE, reclass = FALSE),
    "cannot vectorize if 'reclass = FALSE'"
  )

  expect_error(
    ts_bind(1, 2),
    "at least one object must be ts-boxable"
  )

  expect_error(
    ts_c(a, b),
    "\\[id\\] columns are not identical"
  )

  expect_error(
    ts_chain(ts_c(mdeaths, fdeaths), ts_c(mdeaths, fdeaths)),
    "only single series can be chain-linked"
  )

  expect_error(
    ts_frequency(fdeaths, aggregate = "blabla"),
    "'aggregate' must be one of"
  )

  expect_error(
    ts_frequency(fdeaths, aggregate = mdeaths),
    "'aggregate' must be of class"
  )

  expect_error(
    ts_ggplot(tidyr::crossing(ts_tbl(mdeaths), id = as.character(1:50))),
    "50 time series supplied. Maximum is 29"
  )

  expect_error(
    ts_index(mdeaths, base = c(1974, 1975, 1976)),
    "'base' must be of length 1 or 2, or NULL"
  )

  w <- ts_wide(ts_tbl(ts_c(mdeaths, fdeaths))) %>%
    select(mdeaths, fdeaths, time)
  expect_error(
    ts_long(w),
    "no \\[value\\] column"
  )



  expect_error(
    ts_pick(ts_c(mdeaths, fdeaths), "ttt"),
    "values missing in data: ttt"
  )

  expect_error(
    ts_regular(mdeaths, fill = 1:2),
    "'fill' must be of length 1"
  )

  expect_error(
    ts_span(mdeaths, start = 1:2),
    "must be of length 1"
  )

  expect_error(
    ts_span(mdeaths, end = 1:2),
    "must be of length 1"
  )

  mx <- ts_na_omit(ts_tbl(ts_c(mdeaths, austres)))
  expect_error(
    ts_span(mx, end = 1),
    "mixed frequencies: 'end' cannot be specified as integer"
  )
  expect_error(
    ts_span(mx, start = 1),
    "mixed frequencies: 'start' cannot be specified as integer"
  )

  expect_error(
    ts_span(mdeaths, start = 1910, end = 1920),
    "span contains no dat"
  )

})


test_that("messages work as expected", {
  # in order of search for 'message(''

  x <- ts_tbl(mdeaths)
  x <- rename(x, value2 = value)
  x$one_more <- 1

  expect_message(
    z <- ts_dts(x) ,
    "Are you using a wide data frame?"
  )

  skip_if_not_installed("tsibble")
  skip_on_cran()
  expect_message(
    ts_tbl(tsibbledata::nyc_bikes),
    "ignoring non-numeric measure"
  )

  # Cannot produce the message in ts_c()
  # "cannot convert output to class '"

  expect_message(
    ts_frequency(irreg1),
    "series is not regular, 'na.rm' is set to TRUE."
  )

  wl <- ts_wide(ts_tbl(ts_c(mdeaths, fdeaths))) %>%
    tidyr::crossing(id = c("A", "B")) %>%
    relocate(id, 1) %>%
    arrange(id, time)
  expect_message(
    ts_long(wl),
    "Additional \\[id\\] column"
  )

  expect_message(
    ts_dts(wl[, c(1, 3, 4, 2)]),
    "Are you using a wide data frame?"
  )

  expect_message(
    ts_trend(irreg1[1:4,]),
    "o trend estimation for series with less than 7 obs."
  )

})


