# dplyr::near
is_near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}
