

as_ <- function(x = "xts"){
  # print(x)
  stopifnot(x %in% c("xts", "ts", "data.frame", "data.table"))
  get(paste0("as_", x))
}

desired_class <- function(ll){
  z <- unique(vapply(ll, relevant_class, ""))
  if (length(z) == 1){
    if (z == "ts"){
      # no "ts" if mixed frequecies
      if (length(unique(vapply(ll, frequency, 1))) > 1) return("xts")
    }
    return(z)
  } else {
    return("xts")
  }
}


relevant_class <- function(x){
  if (inherits(x, "ts")){
    return("ts")
  }
  if (inherits(x, "xts")){
    return("xts")
  }
  if (inherits(x, "data.table")){
    return("data.table")
  }
  if (inherits(x, "data.frame")){
    return("data.table")
  }
}




time_to_date <- function(x){
  stopifnot(inherits(x, "ts"))

  ti <- time(x)
  f <- frequency(x)

  if (f > 370){
    stop("time to POXIXct not yet implemented")
  }

  st <- floor(ti[1])
  en <- floor(ti[length(ti)])

  z <- seq(as.Date(paste0(st, "-01-01")), 
  as.Date(paste0(en, "-12-31")), by = "day")
  z0 <- seq(st, en + 1, length.out = length(z) + 1)[-(length(z) + 1)] - 0.003  # leap year tweak
  stopifnot(length(z0) == length(z))

  z[findInterval(ti, z0)]
}






date_to_time <- function(x){
  stopifnot(inherits(x, "xts"))

  id <- index(x)

  p <- periodicity(x)

  if (p$scale == "daily"){
    f = 365.25
  } else {
    stop("TODO")
  }

  id

  st <- as.numeric(format(id[1], "%Y"))
  en <- as.numeric(format(id[length(id)], "%Y"))

  z <- seq(as.Date(paste0(st, "-01-01")), 
  as.Date(paste0(en, "-12-31")), by = "day")
  z0 <- seq(st, en + 1, length.out = length(z) + 1)[-(length(z) + 1)] + 0.003  # leap year tweak
  stopifnot(length(z0) == length(z))

  z0[findInterval(id, z)]
}






