require(moments)
require(data.table)
#' Computes stats useful for right-skewed, heavy-tailed, zero-inflated variables
#' @param cols a character vector of column names
#' @param DT   data.table object containing cols
#' @return  a data.table object containing the summary stats
#' @export
dTail = function(cols, DT) {
  IDA = DT[, .(var      = colnames(.SD),                   ## .SD = compact syntax for executing on each separate column
               pctNA    = 100*colMeans(is.na(.SD)),
               pct0     = 100*colMeans(.SD==0, na.rm=T),   ## pct of non-missing values
               nlev     = sapply(.SD, uniqueN),            ## data.table::uniqueN
               min      = sapply(.SD, min, na.rm=T),       ## need to sapply vector/matrix functions to the columns of a data.table
               max      = sapply(.SD, max, na.rm=T),
               mean     = colMeans(.SD, na.rm=T),
               skewness = skewness(.SD, na.rm=T),          ## moments::skewness
               exkurt   = kurtosis(.SD, na.rm=T)-3         ## moments::kurtosis, subtract 3 for excess kurtosis
  ), .SDcols=cols]                                         ## specify the columns included in .SD
  return(IDA)
}

#' Create missing indicator for cols in a data.table
#' @param dt a data.table
#' @param cols column names to create missing indicator for, default to all columns
#' @param suffix the suffix to append to the column name to form the name for missing indicator columns
#' @param detect Logical value specifying if missing indicator is created for all columns with missing values
#'
#' @return None. All data manipulation is done in place.
#'
#'
#' @export
create_mi <-function(dt, cols=colnames(dt), suffix='_mi', detect=c(F, T)){
  detect<-match.arg(detect)
  if(detect){
    NAs = col[sapply(cols, function(x) anyNA(dt[,get(x)]))]
    col = names(NAs[NAs==TRUE])
  }
  dt[, eval(paste0(eval(cols), suffix)):=lapply(.SD, is.na), .SDcols = cols ]
  dt[, eval(paste0(eval(cols), suffix)):=lapply(.SD, as.integer), .SDcols = paste0(cols, suffix) ]
}

