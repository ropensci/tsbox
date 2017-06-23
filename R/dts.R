

# Constructor for dts objects

#' @export
dts <- function(x, ...) UseMethod("dts")

#' @export
#' @method dts numeric
dts.numeric <- function(x, time, var){
  z <- data.table(time = time, value = x, var = var)
  add_dts_class(z)
}






# ts_plot(bind_dts(dts(x1), dts(x2)))


# x <- bind_dts(lapply(paste0("var", 1:100), function(e) dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "day"), var = e)))


add_dts_class <- function(x){
  class(x) <- c("dts", class(x))
  x
}

#' @export
bind_dts <- function(...){
  ll <- list(...)
  if (!inherits(ll[[1]], "dts")){
    ll <- ll[[1]]
  }
  is.dts <- vapply(ll, function(e) inherits(e, "dts"), TRUE)
  stopifnot(all(is.dts))  # TODO better message
  z <- rbindlist(ll)
  add_dts_class(z)
}


#' @export
select_dts <- function(x, vars){
  stopifnot(inherits(x, "dts"))
  z <- x[var %in% vars]
  add_dts_class(z)
}

#' @export
window_dts <- function(x, start = NULL, end = NULL){
  if (!is.null(start)) {
    start <- as.Date(start)
    x <- x[time >= start]
  }
  if (!is.null(end)) {
    x <- x[time <= end]
  }
  add_dts_class(x)
}


# x <- bind_dts(lapply(paste0("var", 1:100), function(e) dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "day"), var = e)))
# sx <- spread_dts(x)
# gx <- gather_dts(sx)

# setkey(gx)
# setkey(x)
# all.equal(x, gx)

#' @export
spread_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  dcast(x, time ~ var)
}

#' @export
gather_dts <- function(x){
  stopifnot(inherits(x, "data.table"))
  z <- melt(x, id.vars = "time", variable.name = "var", variable.factor = FALSE)
  setcolorder(z, c("time", "value", "var"))
  add_dts_class(z)
}

