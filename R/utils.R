
#' Universal Converter Function
#' 
#' @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @return returns a function
#' @export
coerce_to_ <- function(x = "xts"){
  # print(x)
  stopifnot(x %in% c("xts", "ts", "data.frame", "data.table", "tbl"))
  get(paste0("ts_", x))
}

desired_class <- function(ll){
  z <- unique(vapply(ll, relevant_class, ""))
  if (length(z) == 1){
    if (z == "ts"){
      # no "ts" if mixed frequecies
      if (length(unique(vapply(ll, frequency, 1))) > 1) return("data.frame")
    }
    return(z)
  } else {
    return("data.frame")
  }
}



#' Extract the Relavant Class
#' 
#' @param x time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @export
relevant_class <- function(x){
  if (inherits(x, "ts")){
    return("ts")
  }
  if (inherits(x, "xts")){
    return("xts")
  }
  if (inherits(x, "data.table")){
    return("data.table")
  }
  if (inherits(x, "tbl")){
    return("tbl")
  }
  if (inherits(x, "data.frame")){
    return("data.frame")
  }
}


var_n <- function(x){
  stopifnot(inherits(x, "dts"))
  length(unique(x[, var]))
}

var_names <- function(x){
  stopifnot(inherits(x, "dts"))
  unique(x[, var])
}