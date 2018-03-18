#' Collect Time Series
#'
#' Collect time series as multiple time series.
#'
#' In data frame objects, multiple time series are stored in a long data frame.
#' In `ts` and `xts` objects, time series are combined horizontally.
#'
#' @param ... ts-boxable time series, objects of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#'
#' @return a ts-boxable object of the same class as the input.
#' If series of different classes are combined, the class of the first series is
#' used (if possible).
#'
#' @seealso [ts_bind], to bind multiple time series to a single series.
#'
#' @examples
#' head(ts_c(ts_df(EuStockMarkets), AirPassengers))
#'
#' # labeling
#' x <- ts_c(
#'   `International Airline Passengers` = ts_xts(AirPassengers),
#'   `Deaths from Lung Diseases` = ldeaths
#' )
#' head(x)
#'
#' @export
ts_c <- function(...) {
  ll <- list(...)

  if (length(ll) == 1) return(ll[[1]])

  call.names <- unlist(lapply(substitute(placeholderFunction(...))[-1], deparse, 
                              width.cutoff = 500L))
  # use name if specified in call
  call.names[names(call.names) != ""] <- names(call.names)[names(call.names) != ""]

  desired.class <- desired_class(ll)

  # special treatment for same frequency ts for speed and accuracy gain
  if (identical(unique(desired.class), "ts")){
    # same frequency?
    if (length(unique(vapply(ll, frequency, 1))) == 1){
      is.unnamed <- vapply(ll, function(e) length(colnames(e)) <= 1, TRUE)
      names.for.unnamed <- call.names[is.unnamed]
      ll.names <- ll
      ll.names[is.unnamed] <- names.for.unnamed
      ll.names[!is.unnamed] <- lapply(ll[!is.unnamed], colnames)
      z <- do.call(cbind, ll)
      colnames(z) <- make.unique(unlist(ll.names))
      return(z)
    }
  }

  ll.dts <- lapply(ll, ts_dts)
  vnames <- lapply(ll.dts, function(e) dts_cname(e)$id)

  cid <- dts_cname(ll.dts[[1]])$id
  # In case first element is unnamed series
  if (length(cid) == 0) {
    cid <- "id"
  }

  # add names from call for single series
  is.unnamed <- vapply(ll.dts, function(e) ncol(e) == 2, FALSE)
  names.for.unnamed <- call.names[is.unnamed]
  # name unnamed
  ll.dts[is.unnamed] <- Map(
    function(dt, id) {
      cname <- dts_cname(dt)
      cname$id <- "id"
      dt$id <- id
      # this will have exactly 3 cols
      setcolorder(dt, c(3, 1, 2))
      setattr(dt, "cname", cname)
      dt
    },
    dt = ll.dts[is.unnamed],
    id = names.for.unnamed
  )

  if (length(unique(lapply(ll.dts, function(e) dts_cname(e)$id))) > 1) {
    stop("if present, id columns must be the same for all objects", call. = FALSE)
  }

  ll.dts <- unify_time_class(ll.dts)

  # ensure id uniqueness (not happy)
  for (cid.i in cid){
    all.levels <- lapply(ll.dts, function(e) unique(e[[cid.i]]))
    # only do if needed
    if (length(unique(unlist(all.levels))) == length(unlist(all.levels))) break
    unique.levels <- character(0)
    for (i in seq_along(all.levels)){
      st <- length(unique.levels) + 1
      en <- length(unique.levels) + length(all.levels[[i]])
      unique.levels <- make.unique(c(unique.levels, all.levels[[i]]))
      all.levels[[i]] <- unique.levels[st:en]
    }
    # set_levels(rep(c("c", "a", "b"), each = 5), c("Chr", "Al", "Be"))
    set_levels <- function(x, names){
      as.character(`levels<-`(as.factor(x), sort(names)))
    }
    set_levels_dt <- function(dt, names){
      dt[[cid.i]] <- set_levels(dt[[cid.i]], names)
      dt
    }
    ll.dts <- Map(set_levels_dt, ll.dts, all.levels)
  }

  z0 <- rbindlist(ll.dts)
  z <- try(as_class(desired.class)(z0), silent = TRUE)

  if (inherits(z, "try-error")) {
    z <- ts_df(z0)
    message("cannot convert output to class '", desired.class, "', returning 'data.frame'")
  } 
  z
}


unify_time_class <- function(ll) {
  cl <- vapply(ll, function(e) dts_tattr(e)$class, "")
  if (length(unique(cl)) > 1) {
    tz <- dts_tattr(ll[[1]])$tz
    make_date_posixct <- function(x) {
      x[[dts_cname(x)$time]] <- as.POSIXct(x[[dts_cname(x)$time]], origin = "1970-01-01", tz = tz)
      x
    }
    ll[cl == "Date"] <- lapply(ll[cl == "Date"], make_date_posixct)
  }
  ll
}
