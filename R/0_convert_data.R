# utility function to find POSIXct range (for coding only)
# find_range <- function(by = "1 month") {
#   ser <- seq(from = as.POSIXct("1900-01-01"), to = as.POSIXct("2020-01-01"), by = by)
#   range(diff(as.numeric(as.POSIXct(ser))))
# }
# find_range("1 hour")

# 365.2425  # Gregorian Year

.mapdiff <- data.table::fread("
freq          , diff      , string  , tol
-1            , 0         , NA      , 0
31556952      , 1         , 1 sec   , 0.1
-1            , 1         , NA      , 0.1
15778476      , 2         , 2 sec   , 0.1
-1            , 2         , NA      , 0.1
6311390       , 5         , 5 sec   , 0.1
-1            , 5         , NA      , 0.1
3155695       , 10        , 10 sec  , 0.1
-1            , 10        , NA      , 0.1
2103797       , 15        , 15 sec  , 0.1
-1            , 15        , NA      , 0.1
1577848       , 20        , 20 sec  , 0.1
-1            , 20        , NA      , 0.1
1051898       , 30        , 30 sec  , 0.1
-1            , 30        , NA      , 0.1
525949.2      , 60        , 1 min   , 1
-1            , 60        , NA      , 1
262974.6      , 120       , 2 min   , 1
-1            , 120       , NA      , 1
105189.8      , 300       , 5 min   , 1
-1            , 300       , NA      , 1
52594.92      , 600       , 10 min  , 1
-1            , 600       , NA      , 1
35063.28      , 900       , 15 min  , 1
-1            , 900       , NA      , 1
26297.46      , 1200      , 20 min  , 5
-1            , 1200      , NA      , 5
17531.64      , 1800      , 30 min  , 5
-1            , 1800      , NA      , 5
8765.82       , 3600      , 1 hour  , 5
-1            , 3600      , NA      , 5
4382.91       , 7200      , 2 hour  , 10
-1            , 7200      , NA      , 10
2921.94       , 10800     , 3 hour  , 10
-1            , 10800     , NA      , 10
2191.455      , 14400     , 4 hour  , 30
-1            , 14400     , NA      , 30
1460.97       , 21600     , 6 hour  , 30
-1            , 21600     , NA      , 30
730.485       , 43200     , 12 hour , 30
-1            , 43200     , NA      , 30
365.2425      , 86400     , 1 day   , 60
-1            , 86400     , NA      , 60
12            , 2419200   , 1 month , 200
-1            , 2682000   , NA      , 200
6             , 5097600   , 2 month , 200
-1            , 5356800   , NA      , 200
4             , 7772400   , 3 month , 200
-1            , 7952400   , NA      , 200
3             , 10364400  , 4 month , 400
-1            , 10627200  , NA      , 400
2             , 15631200  , 6 month , 400
-1            , 15904800  , NA      , 400
1             , 31536000  , 1 year  , 1000
-1            , 31622400  , NA      , 1000
0.5           , 63072000  , 2 year  , 1000
-1            , 63158400  , NA      , 1000
0.33333333333 , 94608000  , 3 year  , 1000
-1            , 94698000  , NA      , 1000
0.25          , 126144000 , 4 year  , 1000
-1            , 126230400 , NA      , 1000
0.2           , 157766400 , 5 year  , 5000
-1            , 157852800 , NA      , 5000
0.1           , 315532800 , 10 year , 10000
-1            , 315619200 , NA      , 10000
")

# add tolerance
.mapdiff[, diff := as.numeric(diff)]
.mapdiff[freq == -1, diff := diff + tol]
.mapdiff[freq != -1, diff := diff - tol]

# utility to detect regular frequencies
frequency_table <- function(x) {

  N <- freq <- share <- string <- NULL

  stopifnot(class(x)[1] %in% c("Date", "POSIXct"))

  # table with unique differences
  diffdt <- data.table(table(diff(as.numeric(as.POSIXct(sort(x))))))
  setnames(diffdt, "V1", "diff")
  diffdt[, diff := as.numeric(diff)]

  # which differences correspond to which frequency?
  i <- cut(
    diffdt$diff, 
    breaks = .mapdiff$diff, 
    labels = FALSE, 
    include.lowest = TRUE
  )

  z0 <- .mapdiff[i][, N := diffdt$N]
  z <- z0[, list(N = sum(N), freq = freq[1]), by = string]
  z[, share := N / (sum(N))]

  z[]
}
