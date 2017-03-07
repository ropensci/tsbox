# #' @export
# na_omit_tail <- function(x){
#   stopifnot(inherits(x, "ts"))
#   if (NCOL(x) == 1){
#     cc <- !is.na(x)
#   } else {
#     cc <- apply(x, 1, function (e) all(complete.cases(e)))
#   }
#   window(x, start = time(x)[cc][1])
# }



check_align <- function(x, y){
  stopifnot(inherits(x, "ts"), inherits(y, "ts"))
  stopifnot(frequency(x) == frequency(y))
  f <- frequency(x)
  if (abs(tsp(x)[2] + 1/f - tsp(y)[1]) > 10e-6){
    stop("x and y are not aligned. x ends: ", 
         paste(end(x), collapse = ":"), ". y starts: ", 
         paste(start(y), collapse = ":")
         )
  }
  return(TRUE)
}




#' @export
tsbind <- function(a, b, check = TRUE){
   if (check) check_align(a, b)
   z <- window(a, end = end(b), extend = TRUE)
   window(z, start = start(b)) <- b
   z
}

#' @export
LEVEL <- function(pc, x){
  check_align(x, pc)
  lx <- lag(x, -1)
  fctr <- pc
  fctr[] <- cumprod(1 + (pc / 100))
  fctr * lx[length(lx)]
}


#' @export
#' @method as.matrix ts
as.matrix.ts <- function(x){
  z <- unclass(x)
  attr(z, "tsp") <- NULL
  as.matrix(z)
}




