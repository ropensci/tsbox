

guess_time_var_value <- function(x){

  time.name = "time"
  var.name = "var"
  value.name = "value"

  x <- x[1:5,]

  cnames <- colnames(x)
  cclasses <- lapply(x, class)

  i.am.in.guessing.mood <- FALSE

  if (!time.name %in% cnames) {
    if (cclasses[1] %in% c("character", "Date", "POSIXct")){
      message("Using first column name '", cnames[1], "' as 'time.name'")
      time.name <- cnames[1]
      i.am.in.guessing.mood <- TRUE
    } else {
      stop('First column not of class "character", "Date" or "POSIXct". To be specific, use a colum named "time".')
    }
  }
  stopifnot(time.name %in% cnames)

  if (!value.name %in% cnames) {
    if ((any(cclasses[[2]] %in% c("numeric", "integer")))){
        value.name <- cnames[2]
        message("Using second column name '", cnames[2], "' as 'value.name'")
    } else {
      stop('Second column does not look like a value column. To be specific, use a colum named "value".')
    }
  }
  stopifnot(value.name %in% cnames)

  if (!var.name %in% cnames) {
    if (NCOL(x) > 2){
      var.name <- cnames[3]
      message("Using thrid column name '", cnames[3], "' as 'var.name'")
      i.am.in.guessing.mood <- TRUE
      stopifnot(var.name %in% cnames)
    }
  }



  list(time.name = time.name,
       var.name = var.name,
       value.name = value.name)
}
