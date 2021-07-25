skip_on_cran()

test_that("ts_first_of_period works", {

  x <- ts_c(
    a = ts_lag(ts_df(mdeaths), "14 days"),
    b = ts_lag(ts_df(mdeaths), "-2 days")
  )
  ans <- ts_first_of_period(x)
  expect_true(all(data.table::mday(ans$time) == 1))

  ans <- ts_first_of_period(ts_lag(ts_df(austres), "14 days"))
  expect_true(all(data.table::mday(ans$time) == 1))

  x <- ts_lag(data.table(
    time = seq(anytime::anytime(1970), length.out = 10, by = "10 sec"),
    value = rnorm(10)
  ), "3 sec")
  ans <- ts_first_of_period(x)

  expect_true(all(as.integer(ans$time) %% 10 == 0))

})

