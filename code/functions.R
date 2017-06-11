require(bit64)        ## recommended add-on for data.table
require(data.table)   ## used for data manipulation
require(moments)      ## used to calculate skewness and kurtosis
require(ggplot2)      ## plotting
require(h2o)
require(pROC)
require(corrplot)
require(polycor)
require(lubridate)

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

top_n<-function(dt, keycols, freqcol, outcol='freq', n=1, descending=F, col_suffix=NULL){
  combinedKeys = c(keycols, freqcol)
  numberOfSlots = seq(1, n)
  dt_freq = dt[, list(hfreq=.N), by= eval(combinedKeys)]
  top_freq = dt_freq[,.SD[order(hfreq, decreasing=descending)[numberOfSlots]], by=eval(keycols)]
  top_freq[, variable:=seq(1,n)]
  setnames(top_freq, "hfreq", outcol)
  #add suffix to the freqcol and outcol if set 
  oriNames = c(freqcol, outcol)
  if(!is.null(col_suffix)){
    newNames = paste(oriNames, col_suffix, sep="_")
    setnames(top_freq, oriNames, newNames)
  }else{
    newNames = oriNames
  }
  dcast_formula = paste(paste(keycols, collapse ="+"), "variable", sep="~")
  top_n_freq = dcast(top_freq, dcast_formula, value.var = newNames)
  return(top_n_freq)
}

tz_transfer<-function(dt, cname, oname='formated_dt', format='ymd_hms', ori_tz = 'EST5EDT', tar_tz='CST6CDT', remove_ori = T){
  # EASE table uses EST as timezone(tz), set accordingly
  if(format == 'ymd_hms'){
    dt[, (oname):=ymd_hms(get(cname),tz=ori_tz)]
    
  }else if(format=='ymd'){
    dt[, (oname):=ymd(get(cname), tz=ori_tz)]
    
  }
  dt[, (oname):= with_tz(get(oname), tzone=target_tz)]
  if(remove_ori==T){
    dt[, (cname):=NULL, with=F]
  }
}

