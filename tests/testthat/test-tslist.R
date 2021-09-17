test_that("tslist works on single series", {
  expect_s3_class(ts_tslist(AirPassengers), "tslist")
})


test_that("tslist of lenght 1 dont have an id", {
  expect_equal(
    ts_dts(ts_pick(ts_tslist(EuStockMarkets), "DAX")),
    ts_dts(ts_pick(EuStockMarkets, "DAX"))
  )
})
