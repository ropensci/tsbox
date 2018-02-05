#' Bind to or several tsboxable objects
#' @name ts_bind
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



