# universal constructor for ts functions

ts_ <- function(FUN = diff, class = "ts", multiple = TRUE){

  all.classes <- c("ts", "mts", "data.frame", "data.table")
  stopifnot(class %in% all.classes)

  # if the function can handle multiple time series
  if (multiple){
    z <- function(x, ...){
      stopifnot(class %in% all.classes)
      desired.class <- relevant_class(x)
      tsn <- tsnames(x)
      z <- FUN(as_(class)(x), ...)
      z <- as_(desired.class)(z)
      z <- settsnames(z, tsn)
      z
    } 
  } else {
     z <- function(x, ...){
      stopifnot(class %in% all.classes)
      desired.class <- relevant_class(x)
      tsn <- tsnames(x)
      z <- tsapply(as_(class)(x), FUN, ...)
      z <- as_(desired.class)(z)
      z <- settsnames(z, tsn)
      z
    } 
  } 
  return(z)
}

tsdiff <- ts_(diff)


# tsdiff(as_xts(AirPassengers))



