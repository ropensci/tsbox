library(testthat)
library(tsbox)

context("Test combine_cols_data.table")

test_that("Works with column names containing spaces",
         {
           DT <-
             data.table::data.table(a = letters,
                                    "a b" = LETTERS,
                                    c = rnorm(length(letters)))

           expect_equivalent(
             names(tsbox:::combine_cols_data.table(DT, c("a", "a b"))),
             c("id", "c")
           )
         })
