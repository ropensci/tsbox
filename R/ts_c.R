# relevant_names <- function(call.names, list) {
#   stopifnot(inherits(call.names, "list"))

#   relevant.names <- call.names

#   for (i in 1:length(call.names)) {
#     # only do this for single series
#     if (length(call.names[[i]]) == 1) {
#       if (is.null(names(call.names)) || names(call.names)[i] == "") {

#         # 3. prio: use variable names if nothing else is given
#         if (colnames(list[[i]])[1] %in% c("", ".unnamed")) {
#           relevant.names[[i]] <- call.names[[i]]
#         } else {
#           # 2. prio: use colnames if given
#           cn <- colnames(list[[i]])

#           # stopifnot(length(cn) == 1)
#           relevant.names[[i]] <- cn
#         }
#       } else {
#         # 1. prio: always use name for single series if given
#         relevant.names[[i]] <- names(call.names)[i]
#       }
#     }
#   }
#   relevant.names
# }




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
#' ts_c(ts_df(EuStockMarkets), AirPassengers)
#' a <- ts_c(EuStockMarkets, mdeaths)
#'
#' # labelling:
#' ts_c(`International Airline Passengers` = ts_xts(AirPassengers),
#'        `Deaths from Lung Diseases` = ldeaths)
#'
#' ts_c(a = mdeaths, AirPassengers)
#' ts_bind(ts_xts(AirPassengers), mdeaths)
#' ts_c(ts_dt(EuStockMarkets), AirPassengers)
#' ts_bind(ts_dt(mdeaths), AirPassengers)
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

  # special treatment for ts
  # - speed gain
  # - solves spacing issue for in series NAs
  # - but its still terrible code
  if (identical(unique(desired.class), "ts")){
    is.unnamed <- vapply(ll, function(e) length(colnames(e)) <= 1, TRUE)
    names.for.unnamed <- call.names[is.unnamed]
    ll.names <- ll
    ll.names[is.unnamed] <- names.for.unnamed
    ll.names[!is.unnamed] <- lapply(ll[!is.unnamed], colnames)
    z <- do.call(cbind, ll)
    colnames(z) <- make.unique(unlist(ll.names))
    return(z)
  }

  ll.dts <- lapply(ll, ts_dts)
  vnames <- lapply(ll.dts, colname_id)

  colname.id <- colname_id(ll.dts[[1]])
  # In case first element is unnamed series
  if (length(colname.id) == 0) {
    colname.id <- "id"
  }

  # add names from call for single series
  is.unnamed <- vapply(ll.dts, function(e) ncol(e) == 2, FALSE)
  names.for.unnamed <- call.names[is.unnamed]
  # name unnamed
  ll.dts[is.unnamed] <- Map(
    function(dt, id) {
      dt$id <- id
      # this will have exactly 3 cols
      setcolorder(dt, c(3, 1, 2))
      dt
    },
    dt = ll.dts[is.unnamed],
    id = names.for.unnamed
  )

  if (length(unique(lapply(ll.dts, colname_id))) > 1) {
    stop("if present, id columns must be the same for all objects")
  }

  ll.dts <- unify_time_class(ll.dts)

  # ensure id uniqueness (not happy)
  for (id.i in colname.id){
    all.levels <- lapply(ll.dts, function(e) unique(e[[id.i]]))
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
      dt[[id.i]] <- set_levels(dt[[id.i]], names)
      dt
    }
    ll.dts <- Map(set_levels_dt, ll.dts, all.levels)
  }

  z <- rbindlist(ll.dts)

  z <- try(as_class(desired.class)(z))

  if (inherits(z, "try-error")) {
    message("Cannot coerce output to class '", class, "', returning data.frame.")
    z <- ts_df(z)
  }
  z
}


unify_time_class <- function(ll) {
  cl <- vapply(ll, function(e) class(e[[2]])[1], "")
  if (length(unique(cl)) > 1) {

    tz <- attr(ll[cl == "POSIXct"][[1]], "tzone")
    ll[cl == "Date"] <- lapply(ll[cl == "Date"], function(e) change_class.data.table(e, colnames(e)[2], "as.POSIXct", tz = tz))
  }
  ll
}
