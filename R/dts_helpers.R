

#' @export
add_dts_class <- function(x){

  # do not copy!
  setattr(x, "class", c("dts", attr(x, "class")))
  # class(x) <- c("dts", class(x))
  x[]
}

#' @export
rm_dts_class <- function(x){
  setattr(x, "class", setdiff(attr(x, "class"), "dts"))
  x[]
}



