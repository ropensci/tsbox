
#' Convert everything to everything
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param ... additional arguments, passed to methods
#' @param time.name time name (default: `"time"`)
#' @param var.name  var name (default: `"var"`)
#' @param value.name  value name (default: `"time"`)
#' @examples
#'
#' # optionally changes column names (data.frame and data.table) methods
#' options(tsbox.time.name = "Date",
#'         tsbox.value.name = "Value",
#'         tsbox.var.name = "Variable"
#'         )
#' 
#' x.ts <- ts_cbind(mdeaths, fdeaths) 
#' x.xts <- ts_xts(x.ts)
#' x.df <- ts_df(x.xts)
#' \dontrun{
#' library(data.table)
#' x.dt <- ts_dt(x.df) 
#' }
#' 
#' @export
#' @importFrom anytime anydate
#' @importFrom stats as.ts frequency loess na.omit optimize predict resid time ts tsp
#' @importFrom utils browseURL
#' 
ts_xts <- function (x, ...) UseMethod("ts_xts")

#' @export
#' @method ts_xts ts
#' @rdname ts_xts
ts_xts.ts <- function(x, ...){
  stopifnot(inherits(x, "ts"))
  ind <- ts_to_Date_POSIXct(x)
  x0 <- unclass(x)
  attr(x0,"tsp") <- NULL

  z <- xts(x0, order.by = ind)
  ts_set_names(z, ts_names(x))

  # m <- as.zoo(x)
  # f <- frequency(x)

  # if (f == 4 ){
  #   index(m) <- zoo::as.Date.yearqtr(index(m))
  # } else if (f == 12){
  #   index(m) <- zoo::as.Date.yearmon(index(m))
  # } else {
  #   index(m) <- time_to_date(x)
  # }
  
  # as.xts(m)
}



# ts_xts.ts <- function(x, ...){
#   stopifnot(inherits(x, "ts"))

#   m <- as.zoo(x)
#   f <- frequency(x)

#   if (f == 4 ){
#     index(m) <- zoo::as.Date.yearqtr(index(m))
#   } else if (f == 12){
#     index(m) <- zoo::as.Date.yearmon(index(m))
#   } else {
#     index(m) <- time_to_date(x)
#   }
  
#   as.xts(m)
# }




#' @export
#' @rdname ts_xts
#' @method ts_xts xts
ts_xts.xts <- function(x, ...){
  x
}


#' @export
#' @rdname ts_xts
#' @method ts_xts data.frame
ts_xts.data.frame <- function(x, 
                              time.name = getOption("tsbox.time.name", "time"), 
                              var.name = getOption("tsbox.var.name", "var"), 
                              value.name = getOption("tsbox.value.name", "value"), ...){
  cnames <- colnames(x)
  if (!time.name %in% cnames) stop("No column '", time.name, "' in data frame", call. = FALSE)

  stopifnot(time.name %in% cnames)

  ts_xts_core <- function(x){
    if (!any(class(x[[time.name]]) %in% c("POSIXct", "Date"))){
      x[[time.name]] <- anytime::anydate(as.character(x[[time.name]]))
    }
    xts(x = x[[value.name]], order.by = x[[time.name]])
  }
  if (var.name %in% cnames){
    if (!value.name %in% cnames) stop("No column '", value.name, "' in data frame", call. = FALSE)
    var <- as.character(x[[var.name]])

    # factor in split causes reordering, thus [unique(var)]
    ll.df <- split(x, var)[unique(var)]
    ll.xts <- lapply(ll.df, ts_xts_core)

    z <- do.call("cbind", ll.xts)
    names(z) <- names(ll.xts)

  } else {
    if (!value.name %in% cnames){
      if (NCOL(x) == 2){
        colnames(x)[!colnames(x) %in% time.name] <- "value"
      }
      if (!value.name %in% cnames) stop("No column '", value.name, "' in data frame.", call. = FALSE)
    }
    z <- ts_xts_core(x)
    names(z) <- deparse(substitute(x))
  }
  
  z
}

#' @export
#' @rdname ts_xts
#' @method ts_xts data.table
ts_xts.data.table <- function(x, 
                              time.name = getOption("tsbox.time.name", "time"), 
                              var.name = getOption("tsbox.var.name", "var"), 
                              value.name = getOption("tsbox.value.name", "value"), ...){
  cnames <- colnames(x)

  if (!time.name %in% cnames) stop("No column '", time.name, "' in data table.", call. = FALSE)

  stopifnot(requireNamespace("data.table"))

  ts_xts_core <- function(x){
    data.table::setcolorder(x, c(time.name, value.name))
    if (!any(class(x[[1]]) %in% c("POSIXct", "Date", "IDate"))){

      x[[1]] <- anytime::anydate(as.character(x[[1]]))
    }
    data.table::as.xts.data.table(x)
  }
  if (var.name %in% cnames){
    if (!value.name %in% cnames) stop("No column '", value.name, "' in data table.", call. = FALSE)

    var <- as.character(x[[var.name]])
    uvar <- unique(var)
    if (length(uvar) > 100){
      stop("too many series.")
    }

    ll.xts <- lapply(split(x[, c(time.name, value.name), with = FALSE], var), 
                     ts_xts_core)[uvar]
    z <- do.call("cbind", ll.xts)
    names(z) <- names(ll.xts)
  } else {
    if (!value.name %in% cnames){
      if (NCOL(x) == 2){
        colnames(x)[!colnames(x) %in% time.name] <- "value"
      }

     if (!value.name %in% cnames) stop("No column '", value.name, "' in data table.", call. = FALSE)
    }
    z <- ts_xts_core(x)
    names(z) <- deparse(substitute(x))
  }
  
  z
}

