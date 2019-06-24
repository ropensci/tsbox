# functions to manipulate expressions used in data.tables

backtick <- function(x) {
  paste0("`", x, "`")
}

by_expr <- function(x) {
  if (length(x) == 0) return(expression(list()))
  parse(text = paste0("list(", paste(backtick(x), collapse = ", "), ")"))
}

