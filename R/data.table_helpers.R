# Function for easier access to data.table

#' Combine Columns in a data.table
#'
#' @param dt `data.table`, or `dts`
#' @param cols character vector, columns to combine
#' @param sep character, separate columns by
#' @examples
#' combine_cols_data.table(data.table(cars), c("speed", "dist"))
#' @noRd
combine_cols_data.table <- function(dt, cols, sep = "_") {
  paste_sep <- function(...) paste(..., sep = sep)
  id <- NULL
  qq <- as.call(c(quote(paste_sep), lapply(cols, as.name)))
  z <- dt[, id := eval(qq)]
  z[, (setdiff(cols, "id")) := NULL] # but this is the right way to do it
  setcolorder(z, c("id", setdiff(names(z), "id")))
  z[]
}


#' Merging dts by Time Column, Using Rolling Joins
#'
#' @param x `data.table`, or `dts`
#' @param y `data.table`, or `dts`
#' @param by.x character, column by which to merge
#' @param by.y character, column by which to merge
#' @noRd
merge_time_date <- function(x, y, by.x = "time", by.y = "time") {
  `__time_seq` <- time.x <- time.y <- NULL

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
  x0[, `__time_seq` := seq_along(time)]
  x0[, time.x := time]
  y0[, time.y := time]
  y0[, time := time - 0.1] # for robustness
  rj <- y0[x0, roll = 1, on = "time"]

  if (!all(x0$`__time_seq` %in% rj$`__time_seq`)) {
    stop("incomplete merge - this should not occur")
  }

  rj[, time := NULL]

  # new time col name comes from x, the rest from y
  setnames(rj, "time.x", by.x)
  new.names <- c(names(x), setdiff(names(y), by.y))
  z <- rj[, new.names, with = FALSE]

  z
}
