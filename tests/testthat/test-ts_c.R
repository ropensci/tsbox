test_that("combining ids with same name works and keeps order", {
  library(dplyr)
  library(tidyr)

  a <-
    tibble(id1 = c("m", "f"), id2 = "A") %>%
    crossing(time = 2000:2003) %>%
    arrange(desc(id1), id2, time) %>%
    mutate(value = 1:n()) %>%
    ts_regular()

  ans <- ts_c(a, a)
  expect_identical(unique(ans$id1), c("m", "f"))
  expect_identical(unique(ans$id2), c("A", "A.1"))
})


