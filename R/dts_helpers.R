

#' @export
add_dts_class <- function(x){
  class(x) <- c("dts", class(x))
  x
}

#' @export
rm_dts_class <- function(x){
  class(x) <- setdiff(class(x), "dts")
  x
}