library(testthat)
library(tsbox)

context("ts_align")


test_that("ts_align works", {
  
  expect_equal(
    ts_align(ts_df(ts_c(mdeaths, fdeaths = ts_window(fdeaths, end = "1977-01-01")))) %>% 
    filter(id == "fdeaths") %>% 
    pull(time) %>% 
    max(),
    ts_end(mdeaths)
  )

  expect_equal(
    ts_union(ts_df(ts_c(mdeaths, fdeaths = ts_window(fdeaths, end = "1977-01-01")))) %>% 
    filter(id == "fdeaths") %>% 
    pull(time) %>% 
    max(),
    ts_end(mdeaths)
  )

  expect_equal(
    ts_align(ts_df(ts_c(mdeaths, fdeaths = ts_window(fdeaths, end = "1977-01-01"))), fill = 100) %>% 
    filter(id == "fdeaths") %>% 
    pull(time) %>% 
    max(),
    ts_end(mdeaths)
  )

  expect_equal(
    ts_union(ts_df(ts_c(mdeaths, fdeaths = ts_window(fdeaths, end = "1977-01-01"))), fill = 100) %>% 
    filter(id == "fdeaths") %>% 
    pull(time) %>% 
    max(),
    ts_end(mdeaths)
  )

  expect_equal(AirPassengers, ts_align(AirPassengers))

})
