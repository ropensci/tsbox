
# TODO Error Handling
#' @srrstatsTODO {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstatsTODO {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstatsTODO {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*


test_that("errors work as expected", {
  irreg1 <- data.frame(
    time = as.POSIXct(c(
      "2000-01-01", "2001-02-01", "2005-03-01", "2007-03-03", "2007-03-05",
      "2007-03-09", "2007-05-03", "2007-09-03"
    )),
    value = 1:8
  )
  expect_error(time_shift(irreg1$time, 1), "cannot be integer")

})


