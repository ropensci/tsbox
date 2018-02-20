
# lower bound for frequency detection
# -1: no regular freq
.mapdiff <- data.table::fread("
freq     , diff      , string  , tol
-1       , 0         , NA      , 0
31540000 , 1         , 1 sec   , 0.1
-1       , 1         , NA      , 0.1
6308000  , 5         , 5 sec   , 0.1
-1       , 5         , NA      , 0.1
3154000  , 10        , 10 sec  , 0.1
-1       , 10        , NA      , 0.1
2102667  , 15        , 15 sec  , 0.1
-1       , 15        , NA      , 0.1
1577000  , 20        , 20 sec  , 0.1
-1       , 20        , NA      , 0.1
1051333  , 30        , 30 sec  , 0.1
-1       , 30        , NA      , 0.1
525600   , 60        , 1 min   , 1
-1       , 60        , NA      , 1
105120   , 300       , 5 min   , 1
-1       , 300       , NA      , 1
52560    , 600       , 10 min  , 1
-1       , 600       , NA      , 1
35040    , 900       , 15 min  , 1
-1       , 900       , NA      , 1
26280    , 1200      , 20 min  , 5
-1       , 1200      , NA      , 5
17520    , 1800      , 30 min  , 5
-1       , 1800      , NA      , 5
8760     , 3600      , 60 min  , 5
-1       , 3600      , NA      , 5
365.25   , 86400     , 1 day   , 60
-1       , 86400     , NA      , 60
12       , 2419200   , 1 month , 200
-1       , 2678400   , NA      , 200
4        , 7776000   , 3 month , 200
-1       , 7948800   , NA      , 200
1        , 31536000  , 1 year  , 1000
-1       , 31622400  , NA      , 1000
0.1      , 315532800 , 10 year , 10000
-1       , 315619200 , NA      , 10000
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
  i <- cut(diffdt$diff, breaks = .mapdiff$diff, labels = FALSE, include.lowest = TRUE)

  z <- .mapdiff[i][, N := diffdt$N]
  z <- z[, list(N = sum(N), freq = freq[1]), by = string]
  z[, share := N / (sum(N))]
  z[]
}
