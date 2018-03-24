

dts_init <- function(x){
  stopifnot(inherits(x, "data.frame"))
  x <- as.data.table(x)
  stopifnot(inherits(x, "data.table"))
  setattr(x, "class", c("dts", attr(x, "class")))
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)
  x[[cname$time]] <- as_time_or_date(x[[cname$time]])
  x <- ts_na_omit(x)
  x
}

dts_rm <- function(x) {
  setattr(x, "class", setdiff(attr(x, "class"), "dts"))
  setattr(x, "cname", NULL)
  setattr(x, "tattr", NULL)
  x
}

dts_cname <- function(x){
  stopifnot(inherits(x, "dts"))
  z <- attr(x, "cname")
  if (is.null(z)){
    z <- guess_cname(x)
    setattr(x, "cname", z)
  }
  z
}

dts_tattr <- function(x){
  stopifnot(inherits(x, "dts"))
  z <- attr(x, "tattr")
  if (is.null(z)){
    z <- guess_tattr(x)
    setattr(x, "tattr", z)
  }
  z
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

# Combine several id columns in one
combine_id_cols <- function(x, sep = "_") {
  stopifnot(inherits(x, "dts"))
  if (NCOL(x) <= 3) return(x)
  cname <- dts_cname(x)
  z <- combine_cols_data.table(x, dts_cname(x)$id, sep = sep)
  cname$id <- "id"
  setattr(z, "cname", cname)
  z
}
