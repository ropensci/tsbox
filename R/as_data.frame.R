#' @export
#' @rdname as_xts
as_tbl <-  function (x, ...) {
  stopifnot(requireNamespace("tibble"))
  tibble::as_data_frame(as_data.frame(x, ...))
}


#' @export
#' @rdname as_xts
as_data.frame <- function (x, ...) UseMethod("as_data.frame")


#' @export
#' @rdname as_xts
as_df <- function (x, ...) {
  as_data.frame(x, ...)
}

#' @import zoo xts
#' @rdname as_xts
#' @export
#' @method as_data.frame xts
as_data.frame.xts <- function(x, 
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
#' @rdname as_xts
#' @method as_data.frame ts
as_data.frame.ts <- function(x, ...){
  as_data.frame(as_xts(x), ...)
}



#' @export
#' @rdname as_xts
#' @method as_data.frame data.frame
as_data.frame.data.frame <- function(x, ...){
  x
}


#' @export
#' @rdname as_xts
#' @method as_data.frame data.table
as_data.frame.data.table <- function(x, ...){
  as.data.frame(x)
}


#' @export
#' @rdname as_xts
#' @method as_data.frame tbl
as_data.frame.tbl <- function(x, ...){
  as.data.frame(x)
}
