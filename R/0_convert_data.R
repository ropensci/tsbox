#' @importFrom data.table ":=" "data.table" "setcolorder" "as.data.table"
#' @importFrom data.table "setattr" "setnames" "rbindlist" "tstrsplit" "copy"
#' @importFrom data.table "dcast" "melt" "setkey" "setkeyv" "setorder" "shift"
NULL


# Make sure data.table knows we know we're using it
.datatable.aware <- TRUE


#' Utility Function to Find POSIXct Range (Coding Only)
#'
#' @noRd
#' @examples
#' find_range("1 hour")
#' # 365.2425  # Gregorian Year
find_range <- function(by = "1 month") {
  ser <- seq(
    from = as.POSIXct("1900-01-01"),
    to = as.POSIXct("2020-01-01"),
    by = by
  )
  range(diff(as.numeric(as.POSIXct(ser))))
}


#' Retrieve Meta Inforamtion
#' @noRd
meta_freq <- function() {
  meta_freq_data[]
}


#' Utility to Detect Regular Frequencies
#' @param x Date, or POSIXct
#' @noRd
frequency_table <- function(x) {
  N <- freq <- share <- string <- NULL

  stopifnot(class(x)[1] %in% c("Date", "POSIXct"))
  check_frequency_detection(x)

  # table with unique differences
  diffdt <- data.table(table(diff(as.numeric(as.POSIXct(sort(x))))))
  setnames(diffdt, "V1", "diff")
  diffdt[, diff := as.numeric(diff)]

  # which differences correspond to which frequency?
  i <- cut(
    diffdt$diff,
    breaks = meta_freq()$diff,
    labels = FALSE,
    include.lowest = TRUE
  )

  z0 <- meta_freq()[i][, N := diffdt$N]
  z <- z0[, list(N = sum(N), freq = freq[1]), by = string]
  z[, share := N / (sum(N))]

  z[]
}
