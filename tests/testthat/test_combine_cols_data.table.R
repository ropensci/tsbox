library(testthat)
library(tsbox)

context("Test combine_cols_data.table")

test_that("Works with column names containing spaces and special characters",
         {
           DT <-
             data.table::data.table(a = letters,
                                    "a b" = LETTERS,
                                    "a;b" = rev(LETTERS),
                                    c = rnorm(length(letters)))

           Output <- tsbox:::combine_cols_data.table(DT, c("a", "a b", "a;b"))

           expect_equivalent(
             names(Output),
             c("id", "c")
           )

           expect_equivalent(Output[1, id],
                             "a_A_Z")
         })


test_that("Parameter sep works",
          {
            DT <-
              data.table::data.table(a = letters,
                                     "a b" = LETTERS,
                                     c = rnorm(length(letters)))

            Output <- tsbox:::combine_cols_data.table(DT, c("a", "a b"),
                                                      sep = " ")

            expect_equivalent(
              names(Output),
              c("id", "c")
            )

            expect_equivalent(Output[1, id],
                              "a A")
          })
