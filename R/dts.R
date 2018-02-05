# dts time series objects

# an internally used time series class, based on data.table

# - faster than xts
# - less dependencies
# - no masking of R base





#' @export
#' @name ts_ts
ts_dts <- function(x, ...) {
  UseMethod("ts_dts")
}

#' @export
ts_dts.dts  <- function(x, ...) {
  x
}

#' @export
ts_dts.numeric <- function(x, time, var, ...){
  z <- data.table(time = time, value = x, var = var)
  add_dts_class(z)
}



# ts_plot(bind_dts(ts_dts(x1), ts_dts(x2)))


# x <- bind_dts(lapply(paste0("var", 1:100), function(e) ts_dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "day"), var = e)))


# x <- bind_dts(lapply(paste0("var", 1:100), function(e) ts_dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "day"), var = e)))
# sx <- spread_core(x)
# gx <- gather_core(sx)

# setkey(gx)
# setkey(x)
# all.equal(x, gx)

#' @export
#' @name ts_ts
ts_c.dts <- function(...){
  ll <- list(...)
  if (!inherits(ll[[1]], "dts")){
    ll <- ll[[1]]
  }
  is.dts <- vapply(ll, function(e) inherits(e, "dts"), TRUE)
  stopifnot(all(is.dts))  # TODO better message
  z <- rbindlist(ll)
  add_dts_class(z)
}






colname_value <- function(x){
  stopifnot(inherits(x, "dts"))
  names(x)[ncol(x)]
}
colname_time <- function(x){
  stopifnot(inherits(x, "dts"))
  names(x)[ncol(x) - 1]
}
colname_id <- function(x){
  stopifnot(inherits(x, "dts"))
  setdiff(names(x), c(colname_value(x), colname_time(x)))
}

