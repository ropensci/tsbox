test_that("ts_default works", {
  df0 <- ts_df(ts_c(mdeaths, fdeaths))
  # non-default colnames
  colnames(df0) <- c("id", "date", "count")
  # switch back to default colnames
  df <- ts_default(df0)

  expect_identical(colnames(df), c("id", "time", "value"))
})
