# functions to manipulate expressions used in data.tables

by_expr <- function(x) {
  as.call(c(quote(list), lapply(x, as.name)))
}

