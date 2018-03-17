dts_init <- function(x){
  stopifnot(inherits(x, "data.frame"))
  x <- as.data.table(x)
  stopifnot(inherits(x, "data.table"))
  setattr(x, "class", c("dts", attr(x, "class")))
  stopifnot(inherits(x, "dts"))
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

