#' @name ts_bind
#' @export
#' @examples
#' ts_chain(ts_window(mdeaths, end = "1975-12-01"), fdeaths)
#' ts_plot(ts_pc(ts_c(
#'   comb = ts_chain(ts_window(mdeaths, end = "1975-12-01"), fdeaths), 
#'   fdeaths
#' )))
ts_chain <- function(...){
  ll <- list(...)

  tsboxable <- vapply(ll, ts_boxable, TRUE)
  stopifnot(all(tsboxable))

  desired.class <- desired_class(ll)

  z <- Reduce(chain_two, ll) 
  coerce_to_(desired.class)(z)
}


# x <- b[[colname_time(b)]] %in% a[[colname_time(a)]]
first_true <- function(x){
  which(cumsum(as.integer(x)) == 1L)[1]
}

last_true <- function(x){
  which(cumsum(as.integer(x)) == sum(as.integer(x)))[1]
}



chain_two <- function(a, b){

  b <- ts_na_omit(ts_dts(b))
  a <- ts_na_omit(ts_dts(a))

  if ((number_of_series(b) > 1) || (number_of_series(a) > 1)){
    stop("only single series can be (currently) chained")
  }

  stopifnot(inherits(b, "dts"), inherits(a, "dts"))

  # unify time class if needed
  cls <- union(class(b[[colname_time(b)]]), class(a[[colname_time(a)]]))
  if ("POSIXct" %in% cls && "Date" %in% cls) {
    b[[colname_time(b)]] <- as.POSIXct(b[[colname_time(b)]])
    a[[colname_time(a)]] <- as.POSIXct(a[[colname_time(a)]])
  }

  # b is longer than a into the future: extraploation
  if (max(b[[colname_time(b)]]) > max(a[[colname_time(a)]])){
    where.in.b <- last_true(b[[colname_time(b)]] %in% a[[colname_time(a)]])
    where.in.a <- a[[colname_time(a)]] %in% b[[colname_time(b)]][where.in.b]
    anchor.a <- a[[colname_value(a)]][where.in.a]
    extra.b <- b[where.in.b:nrow(b)]
    extra.b[[colname_value(b)]] <- extra.b[[colname_value(b)]] / extra.b[[colname_value(b)]][1] * anchor.a
    a <- ts_bind(a, extra.b[-1])
  }

  # b is longer than a into the past: retropolation
  if (min(b[[colname_time(b)]]) < min(a[[colname_time(a)]])){
    where.in.b <- first_true(b[[colname_time(b)]] %in% a[[colname_time(a)]])
    where.in.a <- a[[colname_time(a)]] %in% b[[colname_time(b)]][where.in.b]
    anchor.a <- a[[colname_value(a)]][where.in.a]
    retro.b <- b[1:where.in.b]
    retro.b[[colname_value(b)]] <- retro.b[[colname_value(b)]] / retro.b[[colname_value(b)]][1] * anchor.a
    a <- ts_bind(a[-1], retro.b)
  }

  a
}



