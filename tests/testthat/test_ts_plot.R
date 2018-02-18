library(testthat)
library(tsbox)

context("ts_plot")


test_that("ts_plot works", {

  ts_plot(AirPassengers, title = "AirPassengers", subtitle = "Heyhey") 
  ts_save(device = "pdf")
  ts_save(device = "png")
  ts_save(device = "bmp")
  ts_save(device = "jpeg")
  ts_save(device = "tiff")

  p <- ts_ggplot(AirPassengers, mdeaths)
  p + theme_tsbox() + scale_color_tsbox()

})
