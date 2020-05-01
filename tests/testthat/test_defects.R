library(testthat)
library(tsbox)

context("fixed defects shuld not appear again")

test_that("concatenation when using data.frame format #166 (1)", {
  with_id <- wo_id <- ts_df(mdeaths)
  with_id$id <- "mdeaths"
  expect_identical(unique(ts_c(wo_id, with_id)$id), c("wo_id", "mdeaths"))
})

test_that("pc keeps single id #166 (2)", {
  with_id <- ts_df(mdeaths)
  with_id$id <- "mdeaths"
  expect_identical(unique(ts_pc(with_id)$id), "mdeaths")
  expect_identical(unique(ts_(diff, vectorize = TRUE)(with_id)$id), "mdeaths")
})

test_that("ts to df conversion works with offset #186", {
  a <- ts(c(1, 2, 3), start = 2015, frequency = 12)
  attr(a, "tsp")[1] <- attr(a, "tsp")[1] - 1e-11
  ans <- ts_df(a)
  expect_is(ans, "data.frame")
})

