test_that("ts_plot works", {
  skip_on_cran()

  tf <- tempfile(fileext = ".pdf")
  pdf(file = tf)
  ts_plot(AirPassengers, title = "AirPassengers", subtitle = "Heyhey")
  dev.off()
  unlink(tf)

  # tf <- tempfile(fileext = ".pdf")
  # ts_save(tf, open = FALSE)
  # expect_true(file.size(tf) > 3000)

  # tf <- tempfile(fileext = ".png")
  # ts_save(tf, open = FALSE)
  # expect_true(file.size(tf) > 3000)

  # tf <- tempfile(fileext = ".bmp")
  # ts_save(tf, open = FALSE)
  # expect_true(file.size(tf) > 3000)

  # tf <- tempfile(fileext = ".jpeg")
  # ts_save(tf, open = FALSE)
  # expect_true(file.size(tf) > 3000)

  # tf <- tempfile(fileext = ".tiff")
  # ts_save(tf, open = TRUE)
  # expect_true(file.size(tf) > 3000)

  p <- ts_ggplot(AirPassengers, mdeaths) + theme_tsbox() + scale_color_tsbox()
  expect_s3_class(p, "ggplot")
})
