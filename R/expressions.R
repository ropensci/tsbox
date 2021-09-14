#' Construct `by` Expression
#'
#' To be used in data.table()
#'
#' @param character grouping variables
#' @examples
#' by_expr(c("a", "b"))
#' @noRd
by_expr <- function(x) {
  as.call(c(quote(list), lapply(x, as.name)))
}
