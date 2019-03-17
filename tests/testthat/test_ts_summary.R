
library(testthat)
library(tsbox)


test_that("ts_summary works with irregular series", {

  skip_on_cran()
  x <- ts_c(
    mdeaths,
    irreg = data.frame(
      time = as.POSIXct(c("2000-01-01", "2001-01-01", "2005-03-03", "2007-03-03", "2007-03-05", "2007-03-09", "2007-05-03", "2007-09-03")),
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
