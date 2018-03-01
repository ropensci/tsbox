add_dts_class <- function(x) {
  # do not copy!
  setattr(x, "class", c("dts", attr(x, "class")))
  # class(x) <- c("dts", class(x))
  x[]
}

rm_dts_class <- function(x) {
  setattr(x, "class", setdiff(attr(x, "class"), "dts"))
  x[]
}

number_of_series <- function(x) {
  stopifnot(inherits(x, "dts"))
  cid <- dts_cname(x)$id
  if ((length(cid)) == 0) {
    1
  } else {
    dt.id <- x[, cid, with = FALSE]
    nrow(unique(dt.id))
  }
}

colname_value <- function(x) {
  stopifnot(inherits(x, "dts"))
  dts_cname(x)$value
  # names(x)[ncol(x)]
}
colname_time <- function(x) {
  stopifnot(inherits(x, "dts"))
  dts_cname(x)$time

  # names(x)[ncol(x) - 1]
}
colname_id <- function(x) {
  stopifnot(inherits(x, "dts"))
  dts_cname(x)$id

  # setdiff(names(x), c(colname_value(x), colname_time(x)))
}

# Combine several id columns in one
combine_id_cols <- function(x) {
  stopifnot(inherits(x, "dts"))
  if (NCOL(x) <= 3) return(x)
  combine_cols_data.table(x, dts_cname(x)$id)
}
