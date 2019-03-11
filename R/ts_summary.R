#' Time Series Properties
#'
#' @inherit ts_dts
#' @param spark.width integer, with of the spark graph, in numer of character.
#'   Set to `NULL` to turn off.
#'
#' @export
#' @examples
#' ts_summary(ts_c(mdeaths, austres))
#' # Extracting specific properties
#' ts_summary(AirPassengers)$start
#' ts_summary(AirPassengers)$freq
#' ts_summary(AirPassengers)$obs
#' @export
ts_summary <- function(x, spark.width = 15) {
  stopifnot(ts_boxable(x))

  x.dts <- ts_dts(ts_default(x))
  # support multi id
  cid <- dts_cname(x.dts)$id
  if (length(cid) == 0) {
    x.dts$id <- deparse(substitute(x))
    setcolorder(x.dts, c("id", "time", "value"))
    cid <- "id"
  }

  .by <- parse(text = paste0("list(", paste(cid, collapse = ", "), ")"))

  ans.freq <- x.dts[, frequency_one(time), by = eval(.by)]
  ans.freq[, c("share", "N") := NULL]
  setnames(ans.freq, "string", "freq_str")
  regular.series <- ans.freq[!is.na(freq)][, cid, with = FALSE]

  ans.other <- x.dts[,list(
    obs = length(na.omit(value)),
    start = min(time),
    end = max(time)
  ), by = eval(.by)]

  # some stuff can be done for regular series only
  ans.regular <- ts_regular(ts_na_omit(x.dts[regular.series, on = cid]))[,
    list(spark_line = ts_spark_core(x = value, spark.width = spark.width)),
    by = eval(.by)
  ]

  ans <- ans.regular[ans.freq[ans.other, on = cid], on = cid]

  setcolorder(ans, c(cid, "obs", "freq_str", "freq", "start", "end", "spark_line"))

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


