test_that("ts_scale does not modify time column", {
  expect_equal(
    ts_c(AirPassengers, EuStockMarkets[, "DAX"])$time,
    ts_scale(ts_c(AirPassengers, DAX = EuStockMarkets[, "DAX"]))$time
  )
})

test_that("POSIXct time col does not get modified.", {
  ap <- ts_df(AirPassengers)
  ap$time <- as.POSIXct(ap$time)
  expect_s3_class(ts_scale(ap)$time, "POSIXct")
})
