# x <- ts_xts(AirPassengers)
# x <- x[-3]
# ll <- ts_rbind(ts_window(AirPassengers, start = "1950-01-01"), ts_window(AirPassengers, end = "1949-12-01"))
# ll <- ts_rbind(mdeaths, AirPassengers)


#' @rdname ts_c
#' @export
ts_rbind <- function(...){

  ll <- list(...)

  if (length(ll) == 1) return(ll[[1]])

  desired.class <- desired_class(ll)

  ll.dts <- lapply(ll, ts_dts)

  

  ll.varnames <- lapply(ll.dts, ts_varnames)

  vname <- ll.varnames[[1]]

  nc <- vapply(ll.dts, ts_nvar, 1L)
  if (length(unique(nc)) != 1){
    stop("Number of variables must be unique but is instead: ", paste(nc, collapse = ", "), call. = FALSE)
  }

  if ((length(vname) > 1) && !all(vapply(ll.varnames, function(e) identical(e, ll.varnames[[1]]), FALSE))){
    stop("Variable names in objects differ. Use ts_set_names() to rename.")
  }

  call.names <- lapply(substitute(placeholderFunction(...))[-1], deparse)
  vname <- unlist(relevant_names(call.names[1], list = ll.dts[1]))
  # vname <- vnames[[1]]


  z <- ll.dts[[1]]

  time.name <- colnames(z)[1]
  time.class <- class(z[[1]])[1]
  ind0 <- ll.dts[[1]][[1]]

  for (i in 2:length(ll.dts)){
    indi <- ll.dts[[i]][[1]]
    dup <- indi %in% ind0
    if (any(dup)) {
      message("duplicate timestamps removed in: ", call.names[[i]])
    }

    z1 <- ll.dts[[i]][!dup]

    if (class(z1[[1]])[1] != time.class){
      if (time.class == "Date") as_fun <- as.Date
      if (time.class == "POSIXct") as_fun <- as.POSIXct
      z1[[1]] <- as_fun(z1[[1]])
    }

    z1 <- ts_set_names(z1, vname)

    z <- rbind(z,  rm_dts_class(z1))
    ind0 <- z[[1]]
  }

  setorder(z, time, var)
 
  coerce_to_(desired.class)(z)



}


