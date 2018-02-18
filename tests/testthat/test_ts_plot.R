library(testthat)
library(tsbox)

context("ts_plot")


test_that("ts_plot works", {

  ts_plot(AirPassengers, title = "AirPassengers", subtitle = "Heyhey") 
  ts_save("pdf")
  ts_save("png")
  ts_save("bmp")
  ts_save("jpeg")
  ts_save("tiff")




p <- ts_ggplot(AirPassengers, mdeaths)
p + theme_tsbox() + scale_color_tsbox()

  
})
