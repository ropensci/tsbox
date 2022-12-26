#' Time Series Properties
#'
#' Extract time series properties, such as the number of observations
#' (`obs`), the time differences between observations (`obs`), the number
#' of observations per year (`freq`), and the start time stamp (`start`)
#' and the end time stamp (`end`) of the series.
#'
#' @inherit ts_default
#' @param spark logical should an additional column with a spark-line added to
#'   the data frame (experimental, ASCII only on Windows.)
#'
#' @return `ts_summary` returns a `data.frame`. Individual column can be
#'   accessed through the `$` notation (see examples).
#'
#' @export
#' @examples
#' ts_summary(ts_c(mdeaths, austres))
#' ts_summary(ts_c(mdeaths, austres), spark = TRUE)
#' # Extracting specific properties
#' ts_summary(AirPassengers)$start
#' ts_summary(AirPassengers)$freq
#' ts_summary(AirPassengers)$obs
#' @export
ts_summary <- function(x, spark = FALSE) {
  freq <- NULL
  value <- NULL
  check_ts_boxable(x)

  # implicit NAs for most operations
  x.dts <- ts_na_omit(ts_dts(ts_default(x)))
  # support multi id
  cid <- dts_cname(x.dts)$id
  if (length(cid) == 0L) {
    x.dts$id <- deparse(substitute(x))
    setcolorder(x.dts, c("id", "time", "value"))
    cid <- "id"
  }

  .by <- by_expr(cid)

  ans.freq <- x.dts[, frequency_one(time), by = eval(.by)]
  ans.freq[, c("share", "N") := NULL]
  setnames(ans.freq, "string", "diff")
  regular.series <- ans.freq[!is.na(freq)][, cid, with = FALSE]

  ans.other <- x.dts[, list(
    obs = length(na.omit(value)),
    start = min(time),
    end = max(time)
  ), by = eval(.by)]

  if (spark) {
    if (.Platform$OS.type == "windows") {
      spark_fun <- spark_ascii
    } else {
      spark_fun <- spark_unicode
    }
    x.dts.regular <- x.dts[regular.series, on = cid]

    # some stuff can be done for regular series only
    if (nrow(x.dts.regular) > 0) {
      x.dts.regular <- ts_span(
        x.dts.regular,
        start = min(x.dts.regular$time, na.rm = TRUE),
        end =  max(x.dts.regular$time, na.rm = TRUE),
        extend = TRUE
      )[,
        list(spark_line = spark_fun(x = value, spark.width = 15)),
        by = eval(.by)
      ]
    } else {
      x.dts.regular$spark_line <- NA_character_
    }


    ans <- x.dts.regular[ans.freq[ans.other, on = cid], on = cid]
    setcolorder(
      ans,
      c(cid, "obs", "diff", "freq", "start", "end", "spark_line")
    )
  } else {
    ans <- ans.freq[ans.other, on = cid]
    setcolorder(
      ans,
      c(cid, "obs", "diff", "freq", "start", "end")
    )
  }


  as.data.frame(ans)
}

# inspired by the sparklines in skimr
# https://github.com/ropensci/skimr/blob/master/R/stats.R
# intToUtf8(braille.map)
braille.map <- setNames(
  c(
    10432L, 10336L, 10320L, 10312L, 10372L, 10276L, 10260L, 10252L,
    10370L, 10274L, 10258L, 10250L, 10369L, 10273L, 10257L, 10249L,
    10368L, 10272L, 10256L, 10248L, 10304L, 10244L, 10242L, 10241L, 32L
  ),
  c(
    "11", "12", "13", "14", "21", "22", "23", "24", "31", "32",
    "33", "34", "41", "42", "43", "44", " 1", " 2", " 3", " 4",
    "1 ", "2 ", "3 ", "4 ", "  "
  )
)


#' Console Representation for Time Series (Unicode)
#'
#' @param x numeric vector
#'
#' @returns character, representing time series in unicode
#' @examples
#' spark_unicode(mdeaths)
#'
#' @noRd
spark_unicode <- function(x, spark.width = 15) {
  cat.y <- cut(
    seq(0, 1, length.out = length(x)),
    seq(0, 1, by = 1 / (2 * spark.width)),
    include.lowest = TRUE,
    labels = FALSE
  )
  x.agg <- tapply(x, cat.y, mean, na.rm = TRUE)
  rr <- range(x.agg, na.rm = TRUE)
  scaled <- (x.agg - rr[1]) / (rr[2] - rr[1])
  cat.scaled <- as.character(
    findInterval(scaled, c(0, 0.25, 0.5, 0.75, 1), all.inside = TRUE)
  )
  cat.scaled[is.na(cat.scaled)] <- " "
  m <- matrix(cat.scaled, ncol = 2, byrow = TRUE)
  cat.scaled.grouped <- paste0(m[, 1], m[, 2])
  intToUtf8(braille.map[cat.scaled.grouped])
}


ascii.map <- setNames(c("_", ".", "-", "\"", " "), c("1", "2", "3", "4", " "))

#' Console Representation for Time Series (ASCII)
#'
#' unicode does not (yet?) work in R data.frame in Windows?
#'
#' @param x numeric vector
#'
#' @returns character, representing time series in ASCII
#' @examples
#' spark_ascii(mdeaths)
#'
#' @noRd
spark_ascii <- function(x, spark.width = 15) {
  cat.y <- cut(
    seq(0, 1, length.out = length(x)),
    seq(0, 1, by = 1 / (spark.width)),
    include.lowest = TRUE,
    labels = FALSE
  )
  x.agg <- tapply(x, cat.y, mean, na.rm = TRUE)
  rr <- range(x.agg, na.rm = TRUE)
  scaled <- (x.agg - rr[1]) / (rr[2] - rr[1])
  cat.scaled <- as.character(
    findInterval(scaled, c(0, 0.25, 0.5, 0.75, 1), all.inside = TRUE)
  )
  cat.scaled[is.na(cat.scaled)] <- " "
  paste(ascii.map[cat.scaled], collapse = "")
}
