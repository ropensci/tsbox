#' Time Series Properties
#'
#' @inherit ts_dts
#' @param spark.width integer, with of the spark graph, in numer of character.
#'   Set to `NULL` to turn off.
#'
#' @export
#' @exapmle
#' ts_summary(ts_c(mdeaths, austres))
#' # Extracting specific properties
#' ts_summary(AirPassengers)
#' @export
ts_summary <- function(x, spark.width = 15) {
  stopifnot(ts_boxable(x))

  # TODO check for regularity, the following only works for regular series

  x.dts <- ts_regular(combine_id_cols(ts_dts(ts_default(x))))
  if (!("id" %in% names(x.dts))) x.dts$id <- deparse(substitute(x))

  x.dts.trimmed <- ts_regular(ts_na_omit(x.dts))

  ans <- x.dts.trimmed[,list(
    obs = length(time),
    freq = frequency_one(time)$string,
    start = min(time),
    end = max(time)
  ), by = id]

  if (!is.null(spark.width)) {
    spark <- x.dts[,list(
      spark = ts_spark_core(x = value, spark.width = spark.width) #,
    ), by = id]

    ans[, spark_line := spark$spark]
  }

  as.data.frame(ans)

}


ts_spark_core <- function(x, spark.width)  {
  cat.y <- cut(
    seq(0, 1, length.out = length(x)),
    seq(0, 1, by = 1 / (2 * spark.width)),
    include.lowest = TRUE,
    labels = FALSE
  )

  x.agg <- tapply(x, cat.y, mean, na.rm = TRUE)
  rr <- range(x.agg, na.rm = TRUE)

  x.agg.scaled <- (x.agg - rr[1])/(rr[2] - rr[1])

  # https://github.com/ropensci/skimr/blob/master/R/stats.R
  braille <- function(x) {
      x <- c(7L, 3L, 2L, 1L, 8L, 6L, 5L, 4L)[x]
      raised <- 1:8 %in% x
      val <- 10240 + sum(raised * 2^(0:7))
      intToUtf8(val)
  }
  stopifnot(is.numeric(x))
  y <- findInterval(x.agg.scaled, seq(0, 1, length.out = 5), all.inside = TRUE)
  ind <- matrix(y, ncol = 2, byrow = TRUE)
  ind[, 2] <- ind[, 2] + 4
  chars <- apply(ind, 1, braille)
  paste(chars, collapse = "")
}


