test_that("ts_bind works as it should.", {
  expect_equal(
    AirPassengers,
    ts_bind(
      ts_span(AirPassengers, start = "1950-01-01"),
      ts_span(AirPassengers, end = "1949-12-01")
    )
  )
  expect_equal(
    ts_dt(AirPassengers),
    ts_bind(
      ts_span(ts_dt(AirPassengers), start = "1950-01-01"),
      ts_span(ts_dt(AirPassengers), end = "1949-12-01")
    )
  )
  expect_equal(
    ts_df(AirPassengers),
    ts_bind(
      AirPassengers = ts_span(ts_df(AirPassengers), start = "1950-01-01"),
      ts_span(ts_df(AirPassengers), end = "1949-12-01")
    )
  )
  expect_equal(
    ts_tbl(AirPassengers),
    ts_bind(
      AirPassengers = ts_span(ts_tbl(AirPassengers), start = "1950-01-01"),
      ts_span(ts_tbl(AirPassengers), end = "1949-12-01")
    )
  )

  expect_s3_class(ts_bind(ts_dt(mdeaths), AirPassengers), "data.table")

  expect_equal(
    c(ts_span(ts_bind(mdeaths, 1:10), start = "1980-09-01")),
    c(9, 10)
  )
})


test_that("ts_chain gives correct results", {
  x <- ts_chain(
    ts_span(mdeaths, start = "1975-01-01", end = "1975-12-01"),
    fdeaths
  )

  expect_equal(
    sum(ts_span((ts_pc(x) - ts_pc(fdeaths)), start = "1976-01-01")),
    0
  )
  expect_equal(
    sum(ts_span((ts_pc(x) - ts_pc(fdeaths)), end = "1974-12-01"), na.rm = TRUE),
    0
  )

  x.df <- ts_chain(
    ts_span(ts_df(mdeaths), start = "1975-01-01", end = "1975-12-01"),
    ts_df(fdeaths)
  )
  x.xts <- ts_chain(
    ts_span(ts_xts(mdeaths), start = "1975-01-01", end = "1975-12-01"),
    ts_xts(fdeaths)
  )
  x.tbl <- ts_chain(
    ts_span(ts_tbl(mdeaths), start = "1975-01-01", end = "1975-12-01"),
    ts_tbl(fdeaths)
  )

  expect_equal(x.df, ts_df(x))
  expect_equal(x.xts, ts_xts(x))
  expect_equal(x.tbl, ts_tbl(x))
})


test_that("ts_bind works with scalars", {
  expect_equal(as.numeric(window(ts_bind(mdeaths, 1), start = 1980)), 1)
  twoseries <- ts_bind(ts_c(mdeaths, fdeaths), 1)
  expect_equal(as.numeric(window(twoseries, start = 1980)[, "mdeaths"]), 1)
  expect_s3_class(ts_bind(EuStockMarkets, 1), "ts")
})

test_that("ts_bind works with short series and scalars (#197)", {
  ans <- ts_bind(ts_tbl(mdeaths)[1:1, ], 1)
  expect_s3_class(ans, "tbl_df")
})

#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
test_that("ensure values are appropriately pre-processed regardless of class structures.", {
  x <- c(2, 2)
  class(x) <- "myclass"
  ans <- ts_bind(mdeaths, x)
  expect_s3_class(ans, "ts")
})






