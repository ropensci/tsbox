
library(testthat)
library(tsbox)


test_that("ts_summary works with irregular series", {

  skip_on_cran()
  x <- ts_c(
    mdeaths,
    irreg = data.frame(
      time = as.POSIXct(c(
        "2000-01-01", "2001-01-01", "2005-03-03", "2007-03-03", "2007-03-05",
        "2007-03-09", "2007-05-03", "2007-09-03"
      )),
      value = 1:8
    )
  )
  smry <- ts_summary(x)
  expect_true(is.na(smry[2, 3]))

})


test_that("ts_summary works with single series", {

  expect_is(ts_summary(AirPassengers), "data.frame")
  expect_is(ts_summary(AirPassengers, spark = TRUE), "data.frame")

})


test_that("ts_summary works with single observations", {
  expect_is(ts_summary(ts_span(mdeaths, -1)), "data.frame")
  expect_is(ts_summary(ts_span(mdeaths, -1), spark = TRUE), "data.frame")
})

test_that("ts_summary works with irregular observations", {
  series_irreg <- tribble(
    ~time,        ~value,
    "1988-01-01", 1,
    "2015-11-01", 2
  ) %>%
  mutate(time = as.Date(time))

  expect_is(ts_summary(series_irreg), "data.frame")
  expect_is(ts_summary(series_irreg, spark = TRUE), "data.frame")
})


