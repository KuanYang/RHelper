dTail = function(cols, DT) {
  ## Computes stats useful for right-skewed, heavy-tailed, zero-inflated variables
  ## args: cols = character vector of column names
  ##       DT   = data.table containing cols
  ## return: data.table containing the summary stats
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

create_mi <-function(dt, cols=colnames(dt), suffix='_mi', detect=F){
  if(detect){
    NAs = col[sapply(cols, function(x) anyNA(dt[,get(x)]))]
    col = names(NAs[NAs==TRUE])
  }
  dt[, eval(paste0(eval(cols), suffix)):=lapply(.SD, is.na), .SDcols = cols ]
  dt[, eval(paste0(eval(cols), suffix)):=lapply(.SD, as.integer), .SDcols = paste0(cols, suffix) ]
  return(dt)  
}

h2o_save_model<-function(tosavedir, model){
  #tosavedir = '/Users/iuf025/GitHub/BestTimeToCall/data/withoutSinceRPC'
  print("saving to: ")
  print(tosavedir)
  dir.create(tosavedir, showWarnings = F)
  h2o.saveModel(object=model, path=tosavedir, force=TRUE)
  h2o.download_mojo(model, path=tosavedir, get_genmodel_jar=TRUE)
  sink(paste(c(tosavedir, "model_detail.txt"), sep='/', collapse = '/'), type='output')
  print(model)
  sink()
}

h2o_auc_in_grid <- function(grid){
  model_ids <- grid@model_ids
  grid_models <- lapply(model_ids, function(model_id) { model = h2o.getModel(model_id) })
  auc <- vector(mode="numeric", length=0)
  for (i in 1:length(grid_models)) {
    auc[i] <- h2o.auc(grid_models[[i]])
  }
  return(auc)
}

