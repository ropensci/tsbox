# Combine several id columns in one
# 
# @param x any time series object
# @param into new column names
combine_cols <- function(x){
  stopifnot(inherits(x, "dts"))
  if (NCOL(x) <= 3) return(x)
  # in a dts, time val is allways at the end
  id.names <- colnames(x)[-(c(-1, 0) + ncol(x))]
  combine_cols_data.table(x, id.names)
}

