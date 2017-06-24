# dts time series objects

# an internally used time series class, based on data.table

# - faster than xts
# - less dependencies
# - no masking of R base





#' Convert everything to everything
#' 
#' @param x a time series object, either `ts`, `data.frame`, `data.table`, `tibble` or `xts`.
#' @param ... additional arguments, passed to methods
#' @param time.name time name (default: `"time"`)
#' @param var.name  var name (default: `"var"`)
#' @param value.name  value name (default: `"time"`)
#' @examples
#'
#' x.ts <- ts_bind(mdeaths, fdeaths) 
#' x.df <- ts_df(x.xts)
#' x.dt <- ts_dt(x.xts)
#' \dontrun{
#' library(xts)
#' x.xts <- ts_xts(x.ts)
#' library(dplyr)
#' x.tbl <- ts_tbl(x.ts)
#' }
#' 
#' @export
#' @import data.table
#' @importFrom anytime anydate
#' @importFrom stats as.ts frequency loess na.omit optimize predict resid time ts tsp
#' @importFrom utils browseURL
#' @import data.table 
ts_dts <- function(x, ...) UseMethod("ts_dts")

#' @export
#' @method ts_dts numeric
ts_dts.numeric <- function(x, time, var){
  z <- data.table(time = time, value = x, var = var)
  add_dts_class(z)
}



# ts_plot(bind_dts(ts_dts(x1), ts_dts(x2)))


# x <- bind_dts(lapply(paste0("var", 1:100), function(e) ts_dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "day"), var = e)))


# x <- bind_dts(lapply(paste0("var", 1:100), function(e) ts_dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "day"), var = e)))
# sx <- spread_dts(x)
# gx <- gather_dts(sx)

# setkey(gx)
# setkey(x)
# all.equal(x, gx)

#' @export
#' @method ts_bind dts
ts_bind.dts <- function(...){
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
#' @method ts_bind dts
ts_select.dts <- function(x, vars){
  stopifnot(inherits(x, "dts"))
  z <- x[var %in% vars]
  add_dts_class(z)
}

#' @export
#' @method ts_bind dts
ts_window.dts <- function(x, start = NULL, end = NULL){
  if (!is.null(start)) {
    start <- as.Date(start)
    x <- x[time >= start]
  }
  if (!is.null(end)) {
    x <- x[time <= end]
  }
  add_dts_class(x)
}

