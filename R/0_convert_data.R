
#' @importFrom data.table ":=" "data.table" "setcolorder" "as.data.table"
#' @importFrom data.table "setattr" "setnames" "rbindlist" "tstrsplit" "copy"
#' @importFrom data.table "dcast" "melt" "setkey" "setkeyv" "setorder"
NULL

# Make sure data.table knows we know we're using it
.datatable.aware <- TRUE


# utility function to find POSIXct range (for coding only)
# find_range <- function(by = "1 month") {
#   ser <- seq(
#     from = as.POSIXct("1900-01-01"),
#     to = as.POSIXct("2020-01-01"),
#     by = by
#   )
#   range(diff(as.numeric(as.POSIXct(ser))))
# }
# find_range("1 hour")

# 365.2425  # Gregorian Year

meta_freq <- function() {
  meta_freq_data[]
}


# utility to detect regular frequencies
frequency_table <- function(x) {

  N <- freq <- share <- string <- NULL

  stopifnot(class(x)[1] %in% c("Date", "POSIXct"))

  if (length(x) < 2) {
    stop(
      "Need at least two timestamps to detect frequency.",
      call. = FALSE
    )
  }

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
