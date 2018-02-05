
number_of_series <- function (x) {
  stopifnot(inherits(x, "dts"))
  colname.id <- colname_id(x)
  if ((length(colname.id)) == 0){
    1
  } else {
    dt.id <- x[, colname.id, with = FALSE]
    nrow(unique(dt.id))
  }
}
