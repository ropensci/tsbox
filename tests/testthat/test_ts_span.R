library(testthat)
library(tsbox)

context("ts_pick")


test_that("ts_pick works", {
    # Programming use
  to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
  a <- ts_pick(EuStockMarkets, .id = to.be.picked.and.renamed)
  b <- ts_pick(EuStockMarkets, `My Dax` = DAX, `My Smi` = SMI)
  expect_equal(a, b)

})
