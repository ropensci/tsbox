    # - dts_frequency()
    # - dts_regular()
    # - dts_start()




    # [X] dts_mode
    # [X] dts_ctime
    # [X] dts_cid
    # [X] dts_cvalue


# x <- dts_init(ts_dt(AirPassengers))
# dts_mode(x)

dts_init <- function(x){
  stopifnot(inherits(x, "data.frame"))
  x <- as.data.table(x)
  stopifnot(inherits(x, "data.table"))
  setattr(x, "class", c("dts", attr(x, "class")))
  x
}

dts_cname <- function(x){
  stopifnot(inherits(x, "dts"))
  z <- attr(x, "cname")
  if (is.null(z)){
    z <- guess_cname(x)
    setattr(x, "cname", z)
  }
  z
}

dts_tattr <- function(x){
  stopifnot(inherits(x, "dts"))
  z <- attr(x, "tattr")
  if (is.null(z)){
    z <- guess_tattr(x)
    setattr(x, "tattr", z)
  }
  z
}




# library(microbenchmark)

# microbenchmark(dts_cname(x)$id)
