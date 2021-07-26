
dts_init <- function(x){

  .SD <- NULL
  stopifnot(inherits(x, "data.frame"))
  x <- as.data.table(x)
  stopifnot(inherits(x, "data.table"))
  setattr(x, "class", c("dts", attr(x, "class")))
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)

  # do not allow duplicates
  is.dup <- duplicated(x[, c(cname$id, cname$time), with = FALSE])
  if (any(is.dup)) {
    z <- as.data.frame(unique(x[is.dup, cname$id, with = FALSE]))
    paste_ <- function(...) paste(..., sep = "_")
    dups <- do.call(paste_, as.list(z))
    if (length(dups) > 0) {
      stop(
        "Object contains series with duplicated information: ",
        paste(dups, collapse = ", "),
        call. = FALSE
      )
    } else {
      stop(
        "Series contains duplicated values in time column: ",
        unique(x[[cname$time]][duplicated(x[[cname$time]])]),
        call. = FALSE
      )
    }
  }

  # new
  setnames(x, cname$time, "time")
  x[, time := as_time_or_date(time)]

  # ensure time is always ordered (if not done before)
  colorder <- names(x)
  .by <- by_expr(dts_cname(x)$id)
  x <- x[, setorder(.SD, time), by = eval(.by)]
  setcolorder(x, colorder)

  setnames(x, "time", cname$time)
  setattr(x, "cname", cname)

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
