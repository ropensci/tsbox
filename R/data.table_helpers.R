# function for easier access to data.table

filter_data.table <- function(DT, column.name, operator = "%in%", filter.value) {
  str <- paste0(column.name, " ", operator, " \"", filter.value, "\"")
  q <- parse(text = str)
  `[`(DT, eval(q))
}

combine_cols_data.table <- function(dt, cols, sep = '_') {
  # probably not the best way to do it
  qq.str <- paste0("id := paste(", paste(cols, collapse = ", "), ", sep = '", sep, "')")
  qq <- parse(text = qq.str)
  z <- dt[, eval(qq)]
  z[, (setdiff(cols, "id")) := NULL] # but this is the right way to do it
  setcolorder(z, c("id", setdiff(names(z), "id")))
  z[]
}


# merging dts over time col, using rolling joins
#
merge_time_date <- function(x, y, by.x = "time", by.y = "time"){

  s <- time.x <- time.y <- NULL

  x0 <- copy(x)
  y0 <- copy(y)

  setnames(x0, by.x, "time")
  setnames(y0, by.y, "time")

  class.x <- class(x0[["time"]])[1]
  class.y <- class(y0[["time"]])[1]

  class <- unique(c(class.x, class.y))
  if (length(class) > 1) {
    x0[["time"]] <- as.POSIXct(x0[["time"]])
    y0[["time"]] <- as.POSIXct(y0[["time"]])
    class <- "POSIXct"
  } 

  # rolling join
  x0[, s := seq_along(time)]
  x0[, time.x := time]
  y0[, time.y := time]
  y0[, time := time - 0.1]  # for robustness
  rj <- y0[x0, roll = 1, on = "time"]

  if (!all(x0$s %in% rj$s)) (stop("incomplete merge"))

  rj[, time := NULL]

  # new time col name comes from x, the rest from y
  setnames(rj, "time.x", by.x)
  new.names <- c(names(x), setdiff(names(y), by.y))
  z <- rj[, new.names, with = FALSE]

  z
}

