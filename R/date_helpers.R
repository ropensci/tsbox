as_time_or_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  # We want to return a date unless its really a time
  anydate(as.character(x))
}

check_regularity <- function(x) {
  stopifnot(inherits(x, "POSIXct"))
  dd <- diff(as.numeric(x))
  if ((max(dd) - min(dd)) > 1000) {
    stop("some dates are not equally spaced.", call. = FALSE)
  }
}






# lower bound for frequency detection
# -1: no regular freq
.mapdiff <- data.table::fread("
freq        diff  
 -1       -10000 
 12      2419200 
 -1      2678400
  4      7776000
 -1      7948800
  1     31536000
 -1     31622400
0.1    315532800
 -1    315619200
")
# add tolerance
tol = 1000L
.mapdiff[freq == -1, diff := diff + tol]
.mapdiff[freq != -1, diff := diff - tol]

regularize_date <- function(x){
  stopifnot(class(x)[1] %in% c("POSIXct", "Date"))

  N <- NULL
  freq <- NULL
  share <- NULL

  x <- as.Date(sort(x))

  # table with unique differences
  diffdt <- data.table(table(diff(as.numeric(as.POSIXct(x)))))
  setnames(diffdt, "V1", "diff")
  diffdt[, diff := as.integer(diff)]

  # which differences correspond to which frequency?
  i <- cut(diffdt$diff, breaks = .mapdiff$diff, labels = FALSE, include.lowest = TRUE)

  diffdt[, freq := .mapdiff[i, freq]]
  diffdt <- diffdt[, list(N = sum(N)), by = freq][, share := N / (sum(N))]

  diffdt[is.na(freq), freq := -1L]

  if (nrow(diffdt[freq == -1]) > 0 && diffdt[freq == -1, share] > 0.5){
    # return NULL if regularization failed
    return(NULL)
  }
  desired.freq <- max(diffdt$freq)

  # largely copy pasted from convert_heuristic. Perhaps factor out
  by.string <- switch(as.character(desired.freq),
    `0.1` = "10 year",
    `1` = "1 year",
    `2` = "6 month",
    `4` = "1 quarter",
    `12` = "1 month"
  )
  z <- seq.Date(from = x[1], to = x[length(x)], by = by.string)
  # return NULL if regularization failed
  if (!all(x %in% z)) return(NULL)
  z
}

