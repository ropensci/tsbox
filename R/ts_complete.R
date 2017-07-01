
# this is a shitty implemenation, should use data.table operations instead

#' @export
ts_complete <- function(x, fill = NA){
  # DONT go for a "ts" object here!!)

  # TODO, do this propperly and move to tsbox

  z <- ts_ts(x)
  if (!is.na(fill)){
    z[is.na(z)] <- fill
  }
  z
  coerce_to_(relevant_class(x))(z)
}
