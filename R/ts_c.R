#' Collect Time Series
#'
#' Collect time series as multiple time series.
#'
#' In data frame objects, multiple time series are stored in a long data frame.
#' In `ts` and `xts` objects, time series are combined horizontally.
#'
#' @param ... ts-boxable time series, an object of class `ts`, `xts`, `zoo`,
#'   `zooreg`, `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`,
#'   `irts` or `timeSeries`.
#' @return a ts-boxable object of the same class as the input, i.e., an object
#'   of class `ts`, `xts`, `zoo`, `zooreg`, `data.frame`, `data.table`, `tbl`,
#'   `tbl_ts`, `tbl_time`, `tis`, `irts` or `timeSeries`.
#'   If series of different classes are combined, the class of the first series
#'   is used (if possible).
#'
#' @seealso [ts_bind], to bind multiple time series to a single series.
#'
#' @examples
#' ts_c(mdeaths, fdeaths)
#' \donttest{
#' ts_c(ts_df(EuStockMarkets), AirPassengers)
#'
#' # labeling
#' x1 <- ts_c(
#'   `International Airline Passengers` = ts_xts(AirPassengers),
#'   `Deaths from Lung Diseases` = ldeaths
#' )
#' head(x1)
#' }
#'
#' @export
ts_c <- function(...) {
  ll <- list(...)

  if (length(ll) == 1L) {
    return(ll[[1]])
  }

  call.names <- unlist(lapply(substitute(placeholderFunction(...))[-1], deparse,
    width.cutoff = 500L
  ))
  # use name if specified in call
  call.names[names(call.names) != ""] <-
    names(call.names)[names(call.names) != ""]

  desired.class <- desired_class(ll)

  # special treatment for same frequency ts for speed and accuracy gain
  if (identical(unique(desired.class), "ts")) {
    # same frequency?
    if (length(unique(vapply(ll, frequency, 1))) == 1L) {
      is.unnamed <- vapply(ll, function(e) length(colnames(e)) <= 1L, TRUE)
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
  if (length(cid) == 0L) {
    cid <- "id"
  }

  # add names from call for single series
  is.unnamed <- vapply(ll.dts, function(e) ncol(e) == 2L, FALSE)
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
    stop0("[id] columns are not identical")
  }

  ll.dts <- unify_time_class(ll.dts)
  ll.dts <- make_ids_unique(ll.dts, cid = cid)

  # make sure the detected ctime and cvalues are used
  cname <- dts_cname(ll.dts[[1]])
  default_colnames <- function(x) {
    cname <- attr(x, "cname")
    setnames(x, cname$time, "time")
    setnames(x, cname$value, "value")
    x
  }
  ll.dts <- lapply(ll.dts, default_colnames)

  z0 <- rbindlist(ll.dts, use.names = TRUE)
  setnames(z0, "time", cname$time)
  setnames(z0, "value", cname$value)
  setattr(z0, "cname", cname)

  z <- try(as_class(desired.class)(z0), silent = TRUE)

  if (inherits(z, "try-error")) {
    z <- ts_df(z0)
    # this probably should not occur
    message(
      "cannot convert output to class '",
      desired.class,
      "', returning 'data.frame'"
    )
  }
  z
}


#' Unify Time Classes in multiple data.tables
#'
#' Date + Date = Date
#' Date + POSIXct = POSIXct
#' POSIXct + POSIXct = POSIXct
#'
#' @param ll list containing list of `data.tables`
#'
#' @noRd
unify_time_class <- function(ll) {
  cl <- vapply(ll, function(e) dts_tattr(e)$class, "")
  if (length(unique(cl)) > 1) {
    tz <- dts_tattr(ll[[1]])$tz
    make_date_posixct <- function(x) {
      x[[dts_cname(x)$time]] <- as.POSIXct(
        x[[dts_cname(x)$time]],
        origin = "1970-01-01",
        tz = tz
      )
      x
    }
    ll[cl == "Date"] <- lapply(ll[cl == "Date"], make_date_posixct)
  }
  ll
}


#' Makes Ids in a List of data.tables Unique
#'
#' - Collect all Ids from multiple columns
#' - Combine them to single Id
#' - Apply make.unique() to single Id
#' - Split again to multiple Id colums
#'
#' @param ll list containing list of `data.tables`
#' @param cid character, Id column names
#'
#' @noRd
make_ids_unique <- function(ll, cid) {
  id <- NULL
  .element <- NULL

  # Collect all Ids from multiple columns
  old.id <- unname(lapply(ll, function(e) unique(e[, cid, with = FALSE])))
  old.id.tab <- rbindlist(old.id, idcol = ".element")

  # Combine them to single Id
  if (length(cid) > 1) {
    old.id.tab <- combine_cols_data.table(old.id.tab, cid, sep = "__cut_here__")
  } else {
    setnames(old.id.tab, cid, "id")
  }

  # Apply make.unique() to single Id
  old.id.tab[, id := make.unique(id)]

  # Split again to multiple Id colums
  z0 <- setNames(tstrsplit(old.id.tab$id, split = "__cut_here__"), cid)
  z <- as.data.table(c(z0, list(.element = old.id.tab[, .element])))
  new.id <- unname(split(z[, cid, with = FALSE], z$.element))

  # Apply new IDs without reordering data
  Map(set_levels_dt, ll, old.id, new.id)
}


#' Set Level in data.table
#'
#' Apply new IDs without reordering data.
#' Ensures that new Ids have the same order as the old Ids
#'
#' @param dt data.table, with data
#' @param old data.table, with unique ids, in any order
#' @param new data.table, with unique ids, in any order
#'
#' @noRd
#' @srrstats {G2.4d} *explicit conversion to factor via `as.factor()`*
#' @srrstats {G2.4e} *explicit conversion from factor via `as...()` functions*
set_levels_dt <- function(dt, old, new) {
  stopifnot(identical(dim(new), dim(old)))
  stopifnot(identical(names(new), names(old)))
  if (isTRUE(all.equal(new, old, check.attributes = FALSE))) {
    return(dt)
  }

  set_levels <- function(x, names) {
    as.character(`levels<-`(as.factor(x), sort(names)))
  }

  for (cid.i in names(new)) {
    if (!identical(old[[cid.i]], new[[cid.i]])) {
      dt[[cid.i]] <- set_levels(dt[[cid.i]], new[[cid.i]])
    }
  }
  dt
}
