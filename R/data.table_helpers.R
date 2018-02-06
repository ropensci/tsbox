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

change_class.data.table <- function(dt, col, operator = "as.POSIXct") {
  # probably not the best way to do it
  qq.str <- paste0(col, " := ", operator, "(", col, ")")
  qq <- parse(text = qq.str)
  z <- dt[, eval(qq)]
  return(z)
}
