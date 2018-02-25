library(testthat)
library(tsbox)

context("ts_plot")


test_that("ts_plot works", {
  ts_plot(AirPassengers, title = "AirPassengers", subtitle = "Heyhey")
  tf <- tempfile(".pdf")
  ts_save(tf, device = "pdf", open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(".png")
  ts_save(tf, device = "png", open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(".bmp")
  ts_save(tf, device = "bmp", open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(".jpeg")
  ts_save(tf, device = "jpeg", open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(".tiff")
  ts_save(tf, device = "tiff", open = TRUE)
  expect_true(file.size(tf) > 3000)

  p <- ts_ggplot(AirPassengers, mdeaths) + theme_tsbox() + scale_color_tsbox()
  expect_is(p, "ggplot")
})
