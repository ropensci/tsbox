
# # old version using ts conversion to get the effect
# ts_complete_old <- function(x, fill = NA){
#   # DONT go for a "ts" object here!!)

#   # TODO, do this propperly

#   z <- ts_ts(x)
#   if (!is.na(fill)){
#     z[is.na(z)] <- fill
#   }
#   ts_reclass(z, x)
# }

# > system.time(ts_complete_old(dt_rg_gmd_agg))
#    user  system elapsed 
#   0.655   0.013   0.670 
# > 
# > system.time(ts_complete(dt_rg_gmd_agg))
#    user  system elapsed 
#   0.311   0.013   0.213 



#' Make all time series the same length
#' @param x any time series object
#' @param fill missign value specifier
#' @export
ts_complete <- function(x, fill = NA){

  x1 <- ts_dts(x)

  if (number_of_series(x1) == 1) return(x)

  # colname.value <- colname_value(x1) 
  colname.time <- colname_time(x1) 
  colname.id <- colname_id(x1) 

  full.time <- unique(x1[, colname.time, with = FALSE])

  # all vars in the data
  full.var <- unique(x1[, colname.id, with = FALSE])
  full.var[, k := 1]  # dummy merge variable
  full.time[, k := 1]

  full.frame <- merge(full.var, full.time, by = "k", allow.cartesian = TRUE)
  full.frame[, k := NULL]

  z <- merge(full.frame, x1, by = colnames(full.frame), all.x = TRUE)

  # this also overwrites existing NAs
  if (!is.na(fill)){
    z[is.na(value), value := fill]
  }

  ts_reclass(z, x)

}
