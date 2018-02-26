# #' Intersection or Union of Time Series (deprecated)
# #'
# #' `ts_intersect` reduces a multiple time series to the intersecting time
# #' stamps. `ts_union` add `NA` to make them the same length.
# #' 
# #' This will be probably removed, as the usefulness in doubtful. For modelling,
# #' `ts_wide` gives you what `ts_union` does, but in a more useful way. On
# #' these wide data frames, we methods to remove NAs, e.g, 
# #' [stats::complete.cases()], [stats::ts.intersect()]
# #' 
# #' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
# #' @return a ts-boxable time series, with the same class as the input.
# #'
# #' @export
# #' @examples
# #' x <- ts_c(mdeaths, fdeaths = ts_window(fdeaths, end = "1977-01-01"))
# #' ts_plot(x, ts_intersect(x) + 50)
# ts_intersect <- function(x) {

#   x1 <- combine_id_cols(ts_dts(x))

#   if (number_of_series(x1) == 1) return(x)

#   ctime <- colname_time(x1)
#   cid <- colname_id(x1)

#   intersect_that_keeps_time_classes <- function (x, y) {
#       unique(y[match(as.vector(x), y, 0L)])
#   }

#   l.time <- split(x1[[ctime]], x1[[cid]])
#   intersect.time <- Reduce(intersect_that_keeps_time_classes, l.time)

#   # # probably no point of having:
#   # union.time <- sort(unique(do.call(c, split(x1[[ctime]], x1[[cid]]))))

#   z <- x1[x1[[ctime]] %in% intersect.time]

#   copy_class(z, x)
# }

