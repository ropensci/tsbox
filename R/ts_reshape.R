#' Reshaping Multiple Time Series
#' 
#' @export
ts_gather <- function (x, ...) UseMethod("ts_gather")

#' @export
#' @rdname ts_gather
#' @method ts_gather ts
ts_gather.ts <- function(x, ...){
  x
}

#' @export
#' @rdname ts_gather
#' @method ts_gather xts
ts_gather.xts <- function(x, ...){
  x
}

#' @export
#' @rdname ts_gather
#' @method ts_gather data.frame
ts_gather.data.frame <- function(x, ...){
  ts_data.frame(gather_core(as.data.table(x), ...))
}

#' @export
#' @rdname ts_gather
#' @method ts_gather data.table
ts_gather.data.table <- function(x, ...){
  ts_data.table(gather_core(x, ...))
}


#' @export
#' @rdname ts_gather
#' @method ts_gather tbl
ts_gather.tbl <- function(x, ...){
  ts_data.table(gather_core(as.data.table(x), ...))
}



#' @export
gather_core <- function(x){
  stopifnot(inherits(x, "data.table"))
  time.name <- guess_time(x)
  z <- melt(x, id.vars = time.name, variable.name = "var", variable.factor = FALSE)
  ts_dts(z)
}






#' @export
ts_spread <- function (x, ...) UseMethod("ts_spread")

#' @export
#' @rdname ts_gather
#' @method ts_spread ts
ts_spread.ts <- function(x, ...){
  x
}

#' @export
#' @rdname ts_gather
#' @method ts_spread xts
ts_spread.xts <- function(x, ...){
  x
}

#' @export
#' @rdname ts_gather
#' @method ts_spread data.frame
ts_spread.data.frame <- function(x, ...){
  ts_data.frame(ts_spread(ts_dts(x), ...))
}

#' @export
#' @rdname ts_gather
#' @method ts_spread data.table
ts_spread.data.table <- function(x, ...){
  ts_data.table(ts_spread(ts_dts(x), ...))
}


#' @export
#' @rdname ts_gather
#' @method ts_spread tbl
ts_spread.tbl <- function(x, ...){
  ts_data.table(ts_spread(ts_dts(x), ...))
}

#' @export
#' @rdname ts_gather
#' @method ts_spread dts
ts_spread.dts <- function(x, ...){
  spread_core(x)
  # ts_data.table(ts_spread(ts_dts(x), ...))
}


#' @export
spread_core <- function(x) {
  stopifnot(inherits(x, "dts"))
  time.name <- colnames(x)[1]
  value.name <- colnames(x)[2]
  var.name <- colnames(x)[3]

  # in a dts, time is always at first position, so no guessing needed

  z <- dcast(x, as.formula(paste(time.name, "~", var.name)), value.var = value.name)
  # keep order as in input
  setcolorder(z, c(time.name, unique(x[[var.name]])))
  z
}


