#' @name ts_bind
#' @export
#' @examples
#' ts_chain(ts_span(mdeaths, end = "1975-12-01"), fdeaths)
#' \donttest{
#' ts_plot(ts_pc(ts_c(
#'   comb = ts_chain(ts_span(mdeaths, end = "1975-12-01"), fdeaths),
#'   fdeaths
#' )))
#' }
ts_chain <- function(...) {
  ll <- list(...)

  tsboxable <- vapply(ll, ts_boxable, TRUE)
  stopifnot(all(tsboxable))

  desired.class <- desired_class(ll)

  z <- Reduce(chain_two, ll)
  as_class(desired.class)(z)
}


#' Position of first TRUE Value
#'
#' @noRd
first_true <- function(x) {
  which(cumsum(as.integer(x)) == 1L)[1]
}


#' Position of last TRUE Value
#'
#' @noRd
last_true <- function(x) {
  which(cumsum(as.integer(x)) == sum(as.integer(x)))[1]
}


#' Chain 2 Time Series
#'
#' Successively called by ts_chain()
#'
#' @param a ts-boxable object
#' @param b ts-boxable object
#'
#' @noRd
chain_two <- function(a, b) {
  b <- ts_dts(b)
  a <- ts_dts(a)

  if ((number_of_series(b) > 1) || (number_of_series(a) > 1)) {
    stop0("only single series can be chain-linked")
  }

  stopifnot(inherits(b, "dts"), inherits(a, "dts"))

  cname <- dts_cname(a)
  cname2 <- dts_cname(b)

  # unify time class if needed
  cls <- union(class(b[[cname2$time]]), class(a[[cname$time]]))
  if ("POSIXct" %in% cls && "Date" %in% cls) {
    b[[cname2$time]] <- as.POSIXct(b[[cname2$time]])
    a[[cname$time]] <- as.POSIXct(a[[cname$time]])
  }

  # b is longer than a into the future: extraploation
  if (max(b[[cname2$time]]) > max(a[[cname$time]])) {
    where.in.b <- last_true(b[[cname2$time]] %in% a[[cname$time]])
    where.in.a <- a[[cname$time]] %in% b[[cname2$time]][where.in.b]
    anchor.a <- a[[cname$value]][where.in.a]
    extra.b <- b[where.in.b:nrow(b)]
    extra.b[[cname2$value]] <- extra.b[[cname2$value]] /
      extra.b[[cname2$value]][1] * anchor.a
    a <- ts_bind(a, extra.b[-1])
  }

  # b is longer than a into the past: retropolation
  if (min(b[[cname2$time]]) < min(a[[cname$time]])) {
    where.in.b <- first_true(b[[cname2$time]] %in% a[[cname$time]])
    where.in.a <- a[[cname$time]] %in% b[[cname2$time]][where.in.b]
    anchor.a <- a[[cname$value]][where.in.a]
    retro.b <- b[1:where.in.b]
    retro.b[[cname2$value]] <- retro.b[[cname2$value]] /
      retro.b[[cname2$value]][nrow(retro.b)] * anchor.a
    a <- ts_bind(a[-1], retro.b)
  }

  a
}
