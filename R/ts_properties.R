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

  x.dts <- ts_regular(combine_id_cols(ts_na_omit(ts_dts(ts_default(x)))))
  if (!("id" %in% names(x.dts))) x.dts$id <- deparse(substitute(x))
  if (is.null(spark.width)) {
    ans <- x.dts[,list(
      obs = length(time),
      freq = frequency_one(time)$string,
      start = min(time),
      end = max(time)
    ), by = id]
  } else {
    ans <- x.dts[,list(
      obs = length(time),
      freq = frequency_one(time)$string,
      start = min(time),
      end = max(time),
      spark = ts_spark_core(x = value, width = spark.width),
      min = min(value),
      median = median(value),
      max = max(value),
    ), by = id]
  }

  as.data.frame(ans)

}


ts_spark_core <- function(x, width)  {
  cat.y <- cut(
    seq(0, 1, length.out = length(x)),
    seq(0, 1, by = 1 / width),
    include.lowest = TRUE,
    labels = FALSE
  )

  x.agg <- tapply(x, cat.y, mean)
  rr <- range(x.agg)
  x.agg.scaled <- (x.agg - rr[1]) / (rr[2] - rr[1])

  bars <- vapply(9601:9608, intToUtf8, character(1))
  # bars <- bars[-c(4, 8)]
  factor <- cut(x.agg.scaled, breaks = seq(0, 1, length.out = length(bars) +
      1), labels = bars, include.lowest = TRUE)
  chars <- as.character(factor)
  chars[is.na(chars)] <- bars[length(bars)]
  paste(chars, collapse = "")
}




