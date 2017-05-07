#' @export
#' @rdname ts_xts
ts_tbl <-  function (x, ...) {
  stopifnot(requireNamespace("tibble"))
  tibble::as_data_frame(ts_data.frame(x, ...))
}


#' @export
#' @rdname ts_xts
ts_data.frame <- function (x, ...) UseMethod("ts_data.frame")


#' @export
#' @rdname ts_xts
ts_df <- function (x, ...) {
  ts_data.frame(x, ...)
}

#' @import zoo xts
#' @rdname ts_xts
#' @export
#' @method ts_data.frame xts
ts_data.frame.xts <- function(x, 
                              time.name = getOption("tsbox.time.name", "time"), 
                              var.name = getOption("tsbox.var.name", "var"), 
                              value.name = getOption("tsbox.value.name", "value"), ...){
  # if (!melt) stop("not yet implemented")

  
  df <- zoo::fortify.zoo(zoo::as.zoo(x), melt = TRUE)
  colnames(df) <- c(time.name, var.name, value.name)
  if (NCOL(x) == 1){
    df[[var.name]] <- NULL
  } else {
    if (is.factor(df[[var.name]])){
      df[[var.name]] <- as.character(df[[var.name]])
    }
  }
  if (any(class(df[[time.name]]) %in% c("yearqtr", "yearmon"))){
    df[[time.name]] <- zoo::as.Date.yearmon(df[[time.name]])
  }

  df
}

#' @export
#' @rdname ts_xts
#' @method ts_data.frame ts
ts_data.frame.ts <- function(x, ...){
  ts_data.frame(ts_xts(x), ...)
}



#' @export
#' @rdname ts_xts
#' @method ts_data.frame data.frame
ts_data.frame.data.frame <- function(x, ...){
  x
}


#' @export
#' @rdname ts_xts
#' @method ts_data.frame data.table
ts_data.frame.data.table <- function(x, ...){
  as.data.frame(x)
}


#' @export
#' @rdname ts_xts
#' @method ts_data.frame tbl
ts_data.frame.tbl <- function(x, ...){
  as.data.frame(x)
}
