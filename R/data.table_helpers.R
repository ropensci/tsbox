# function for easier access to data.table

filter_data.table <- function(DT, column.name, operator = "%in%", filter.value) {
  str <- paste0(column.name, " ", operator, " \"", filter.value, "\"")
  q <- parse(text = str)
  `[`(DT, eval(q))
}

combine_cols_data.table <- function(dt, cols) {
  # probably not the best way to do it
  qq.str <- paste0("id := paste(", paste(cols, collapse = ", "), ", sep = '_')")
  qq <- parse(text = qq.str)
  z <- dt[, eval(qq)]
  z[, (setdiff(cols, "id")) := NULL] # but this is the right way to do it
  setcolorder(z, c("id", setdiff(names(z), "id")))
  z[]
}

# this is inefficient, better do 
# x[['time']] <- as.POSIXct(x[['time']])
change_class.data.table <- function(dt, col, operator = "as.POSIXct", tz = NULL) {
  # probably not the best way to do it

  if (is.null(tz)){
    qq.str <- paste0(col, " := ", operator, "(", col, ")")
  } else {
    qq.str <- paste0(col, " := ", operator, "(", col, ", tz = ", tz, ")")
  }

  qq <- parse(text = qq.str)
  z <- dt[, eval(qq)]
  return(z)
}


# merging dts over time col
#
# if merge should be done over POSIXct, columns are converted to integer and
# converted back after the merge
merge_time_date <- function(x, y, by.x, by.y){
  class.x <- class(x[[by.x]])[1]
  class.y <- class(y[[by.y]])[1]

  class <- unique(c(class.x, class.y))
  if (length(class) > 1) {
    x[[by.x]] <- as.POSIXct(x[[by.x]])
    y[[by.y]] <- as.POSIXct(y[[by.y]])
    class <- "POSIXct"
  } 

  # convert POSIXct to integer, for merge to succeed
  if (class == "POSIXct"){
    tz.x <- attr(x[[by.x]], "tzone")
    tz.y <- attr(x[[by.y]], "tzone")
    tz <- unique(c(tz.x, tz.y, ""))[1]
    x[[by.x]] <- as.integer(x[[by.x]])
    y[[by.y]] <- as.integer(y[[by.y]])
  }

  z <- merge(x, y, by.x = by.x, by.y = by.y, all.x = TRUE)

  # reconvert result to POSIXct
  if (class == "POSIXct"){
    z[[by.x]] <- as.POSIXct(z[[by.x]], origin = "1970-01-01", tz = tz)
  }
  z
}
