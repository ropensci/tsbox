dts_default <- function(x) {
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)
  colorder <- copy(names(x))

  setnames(x, cname$time, "time")
  setnames(x, cname$value, "value")
  setcolorder(x, c(cname$id, "time", "value"))

  cname_default <- cname
  cname_default$time <- "time"
  cname_default$value <- "value"
  setattr(x, "cname", cname_default)

  list(
    x = x,
    cname = cname,
    colorder = colorder
  )
}

dts_restore <- function(x, d) {
  x <- dts_init_minimal(x)
  setnames(x, "time", d$cname$time)
  setnames(x, "value", d$cname$value)
  setcolorder(x, d$colorder)
  setattr(x, "cname", d$cname)
  x
}

dts_init_minimal <- function(x){
  stopifnot(inherits(x, "data.table"))
  if (!inherits(x, "dts")) setattr(x, "class", c("dts", attr(x, "class")))
  x
}
