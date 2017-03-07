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




# #' @export
# LEVEL <- function(pc, x){
#   check_align(x, pc)
#   lx <- lag(x, -1)
#   fctr <- pc
#   fctr[] <- cumprod(1 + (pc / 100))
#   fctr * lx[length(lx)]
# }


# #' @export
# #' @method as.matrix ts
# as.matrix.ts <- function(x){
#   z <- unclass(x)
#   attr(z, "tsp") <- NULL
#   as.matrix(z)
# }




