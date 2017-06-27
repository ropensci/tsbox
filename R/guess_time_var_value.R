
is_time <- function(x){
  if (class(x)[1] %in% c("Date", "POSIXct")) return(TRUE)   # beyond doubt
  # use a short vector for time detection
  if (length(x) > 20){
    x <- c(x[1:3],                                  # first 3
           x[(length(x)%/%2-1):(length(x)%/%2+1)],  # middle 3
           x[(length(x)-2):(length(x))])            # lost 3
  }
  x <- as.character(x)
  all(!is.na(anytime(x)))
}

is_value <- function(x){
  class(x)[1] %in% c("numeric")
}



guess_time <- function(x){
  stopifnot(inherits(x, "data.frame"))
  cnames <- colnames(x)
  if ("time" %in% cnames) return("time")

  z <- NA
  for (cname.i in cnames){
    if (is_time(x[[cname.i]])) {
      z <- cname.i
      break
    }
  }

  if (is.na(z)){
    stop("No time column detected. To be explict, name time column as 'time'.")
  }
  z
}


guess_value <- function(x, time.name = "time"){
  stopifnot(inherits(x, "data.frame"))
  cnames <- colnames(x)
  stopifnot(time.name %in% cnames)
  if ("value" %in% cnames) return("value")

  cnames <- setdiff(cnames, time.name)

  z <- NA
  for (cname.i in cnames){
    if (is_value(x[[cname.i]])) {
      z <- cname.i
      break
    }
  }
  if (is.na(z)){
    stop("No value column detected. To be explict, name value column as 'value'.")
  }
  z
}


# is_var <- function(x){
#   class(x)[1] %in% c("character", "factor")
# }

# guess_var <- function(x, time.name = "time", value.name = "value"){
#   stopifnot(inherits(x, "data.frame"))
#   cnames <- colnames(x)
#   stopifnot(time.name %in% cnames)
#   stopifnot(value.name %in% cnames)

#   if ("var" %in% cnames) return("var")

#   cnames <- setdiff(cnames, c(time.name, value.name))

#   z <- NA
#   for (cname.i in cnames){
#     if (is_var(x[[cname.i]])) {
#       z <- cname.i
#       break
#     }
#   }
  
#   z
# }

guess_time_value <- function(x){
  time.name <- guess_time(x)
  value.name <- guess_value(x, time.name = time.name)
  # var.name <- guess_var(x, time.name = time.name, value.name = value.name)

  c(time.name = time.name,
    value.name = value.name)
}


