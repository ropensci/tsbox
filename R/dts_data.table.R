

guess_time_var_value <- function(x, time.name, var.name, value.name){

  x <- x[1:10,]

  cnames <- colnames(x)
  cclasses <- lapply(x, class)

  i.am.in.guessing.mood <- FALSE

  if (!time.name %in% cnames) {
    if (cclasses[1] %in% c("character", "Date", "POSIXct")){
      message("Using first column name '", cnames[1], "' as 'time.name'")
      time.name <- cnames[1]
      i.am.in.guessing.mood <- TRUE
    } else {
      stop('First column not of class "character", "Date" or "POSIXct", and no "time" column specified')
    }
  }
  stopifnot(time.name %in% cnames)

  if (!var.name %in% cnames) {
    if (NCOL(x) > 2){
      # if (i.am.in.guessing.mood){
        var.name <- cnames[2]
        message("Using second column name '", cnames[2], "' as 'var.name'")
        i.am.in.guessing.mood <- TRUE
      # }
      stopifnot(var.name %in% cnames)
    }
  }

  if (!value.name %in% cnames) {
    if (NCOL(x) == 2){
      if ((any(cclasses[[2]] %in% c("numeric", "integer")))){
        value.name <- cnames[2]
        message("Using second column name '", cnames[2], "' as 'value.name'")
      } else {
        stop("Second column does not look like a value column. Otherwise, be specific about 'value.name'.")
      }
      stopifnot(value.name %in% cnames)
    } else {
      if ((cclasses[3] %in% c("numeric", "integer")) && (i.am.in.guessing.mood)){
        value.name <- cnames[3]
        message("Using third column name '", cnames[3], "' as 'value.name'")
      }
    }
    stopifnot(value.name %in% cnames)
  }

  list(time.name = time.name,
       var.name = var.name,
       value.name = value.name)
}




#' @export
#' @method dts data.table
dts.data.table <- function(x, ...){

  time.name = "time"
  var.name = "var"
  value.name = "value"

  tvv <- guess_time_var_value(x, time.name, var.name, value.name)

  z <- x[, c(tvv[['time.name']], tvv[['value.name']], tvv[['var.name']]), with = FALSE]

  setnames(z, c("time", "value", "var"))
  z[, time := anydate(time)]
  add_dts_class(z)

}







# #' @export
# #' @rdname ts_xts
# #' @method ts_xts data.table
# ts_xts.data.table <- function(x, 
#                               time.name = getOption("tsbox.time.name", "time"), 
#                               var.name = getOption("tsbox.var.name", "var"), 
#                               value.name = getOption("tsbox.value.name", "value"), ...){
#   cnames <- colnames(x)

#   if (!time.name %in% cnames) stop("No column '", time.name, "' in data table.", call. = FALSE)

#   stopifnot(requireNamespace("data.table"))

#   ts_xts_core <- function(x){
#     data.table::setcolorder(x, c(time.name, value.name))
#     if (!any(class(x[[1]]) %in% c("POSIXct", "Date", "IDate"))){

#       x[[1]] <- anytime::anydate(as.character(x[[1]]))
#     }
#     data.table::as.xts.data.table(x)
#   }
#   if (var.name %in% cnames){
#     if (!value.name %in% cnames) stop("No column '", value.name, "' in data table.", call. = FALSE)

#     var <- as.character(x[[var.name]])
#     uvar <- unique(var)
#     if (length(uvar) > 100){
#       stop("too many series.")
#     }

#     ll.xts <- lapply(split(x[, c(time.name, value.name), with = FALSE], var), 
#                      ts_xts_core)[uvar]
#     z <- do.call("cbind", ll.xts)
#     names(z) <- names(ll.xts)
#   } else {
#     if (!value.name %in% cnames){
#       if (NCOL(x) == 2){
#         colnames(x)[!colnames(x) %in% time.name] <- "value"
#       }

#      if (!value.name %in% cnames) stop("No column '", value.name, "' in data table.", call. = FALSE)
#     }
#     z <- ts_xts_core(x)
#     names(z) <- deparse(substitute(x))
#   }
  
#   z
# }

