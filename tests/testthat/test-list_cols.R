# library(tidyr)
# library(dplyr)

# #' @srrstatsTODO {G2.12} *Software should ensure that `data.frame`-like tabular
# #'  objects which have list columns should ensure that those columns are
# #'  appropriately pre-processed either through being removed, converted to
# #'  equivalent vector columns where appropriate, or some other appropriate
# #'  treatment such as an informative error. This behaviour should be tested.*
# test_that("tsbox works with list columns", {

#   x <-
#     ts_tbl(ts_c(mdeaths, fdeaths)) |>
#     nest(data = c(time, value))

#   z <- ts_pick(x, "fdeaths")

#   ts_default(x)

#   # functions that should work with units
#   fl <- lst(
#     ts_bind,
#     ts_c,
#     ts_chain,
#     ts_default,
#     ts_diff,
#     ts_diffy,
#     ts_first_of_period,
#     ts_forecast,
#     ts_index,
#     ts_lag,
#     ts_na_interpolation,
#     ts_pc,
#     ts_pca,
#     ts_regular,
#     ts_seas,
#     ts_span
#   )

#   for (i in seq(fl)){
#     message(names(fl)[i])
#     z <- fl[[i]](x)
#     expect_identical(colnames(z), c("id", "time", "value"))
#   }

# })



