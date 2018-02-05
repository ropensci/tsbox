# ts_chain and ts_bind should be described on the same page

#' Bind to or several tsboxable objects, chaining them together where the first
#' series ends.
#' 
#' @rdname ts_c
#' @export
ts_chain <- function(...){
  ll <- list(...)
  desired.class <- desired_class(ll)

  ll.dts <- lapply(ll, ts_dts)

  stopifnot(ts_nvar(ll[[1]]) == 1)
  z <- Reduce(chain_dts, ll.dts) 
  coerce_to_(desired.class)(z)
}


first_true <- function(x){
  which(cumsum(as.integer(x)) == 1L)[1]
}

chain_dts <- function(new, old){
  stopifnot(inherits(old, "dts"), inherits(new, "dts"))

  old < ts_na_omit(old)
  new < ts_na_omit(new)

  # overlapping time span
  dup <- old[[1]] %in% new[[1]] 

  # first observation of overlapping span
  pos <- first_true(dup)
  anchor <- new[[2]][1]

  retro <- old[1:pos]
  retro[[2]] <- retro[[2]] / retro[[2]][pos] * anchor

  ts_bind(new[-1], retro)
}



#' Bind to or several tsboxable objects
#' 
#' @rdname ts_c
#' @export
ts_bind <- function(...){
  ll <- list(...)
  desired.class <- desired_class(ll)

  ll.dts <- lapply(ll, ts_dts)
  z <- Reduce(bind_dts, ll.dts)
  # setorder(z, time, var)
 
  coerce_to_(desired.class)(z)
}


# Bind two dts objects
bind_dts <- function(a, b) {
  a <- copy(a)
  b <- copy(b)

  stopifnot(inherits(a, "dts"), inherits(b, "dts"))
  
  colname.value <- colname_value(a) 
  colname.time <- colname_time(a) 
  colname.id <- colname_id(a) 

  # temporary, rename back at the end
  setnames(a, colname.time, "time")
  setnames(b, colname_time(b), "time")

  setnames(a, colname.value, "value")
  setnames(b, colname.value, "value_b")

  if (!identical(colname.id, colname_id(b))) {
    stop(
      "Series do not have the same ids: ", 
      paste(colname.id, collapse = ", "), 
      "and", 
      paste(colname_id(b), collapse = ", ")
    )
  }

  z <- merge(a, b, by = c(colname.id, colname.time), all = TRUE)
  # remove key added by merge
  setkey(z, NULL)
  z <- z[is.na(value), value := value_b]
  z[, value_b := NULL]

  # canonical col order
  setcolorder(z, c(setdiff(names(z), c("time", "value")), c("time", "value")))
  
  setnames(z, "time", colname.time)
  setnames(z, "value", colname.value)
  z[]
 
}


