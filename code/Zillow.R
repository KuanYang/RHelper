require(ggplot2)
require(data.table)
require(lubridate)
require(dplyr)
require(h2o)
require(moments)      ## used to calculate skewness and kurtosis
require(rpart)        ## used in section 2 for tree imputation
require(GPArotation)  ## used in section 4
require(plotly)
require(corrplot)
require(stringr)
require(polycor)


wd="~/kaggle/zillow"
setwd(wd)
figdir = paste0(wd, '/figures')
source("code/functions.R")


#renaming
properties = fread("data/properties_2016.csv/properties_2016.csv", colClasses = c(
  rawcensustractandblock = 'character', censustractandblock = 'character',
  fireplaceflag='character'))

properties <- properties %>% rename(
  build_year = yearbuilt,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  storycnt = numberofstories,  
  calcbathcnt = calculatedbathnbr,
  threequarterbathcnt = threequarterbathnbr, 
  hottubflag = hashottuborspa,
  fipsid = fips
)

transactions = fread("data/train_2016.csv")
transactions <- transactions %>% rename(
  date = transactiondate
)
colnames(properties)

#taking care of flags
flagids = grep("flag", colnames(properties), value = T)

properties[,hottubflag:=hottubflag=="true"]
properties[,fireplaceflag:=fireplaceflag=="true"]
properties[,taxdelinquencyflag:= taxdelinquencyflag=='Y']

properties[, (flagids):=lapply(.SD, as.integer), .SDcols=flagids]

#rawcensustractandblock vs censustractandblock
properties[,rawcensustractandblock:=gsub("\\.", "",rawcensustractandblock)]
#after pading, they are the same, except that censustractandblock has some "". replace censustractandblock with rawcensustractandblock and remove censustractandblock
properties[,rawcensustractandblock:=str_pad(rawcensustractandblock, 15, side="right", pad='0')]
properties[,censustractandblock:=rawcensustractandblock]
properties[,rawcensustractandblock:=NULL]

#merge response
TR.all = merge(x=properties, y=transactions, by = 'parcelid', all.x=TRUE)
TR = TR.all[is.na(logerror)==F]

#useless cols
uselesscols = c('longitude', 'latitude')

#id cols to factors
#greedy but works for now
idcols = grep('id|code', colnames(TR), value = T)
#keep parcelid in int
idcols = setdiff(idcols, 'parcelid')
TR[,(idcols):= lapply(.SD, as.factor), .SDcols = idcols]

fwrite(TR, "TR.csv")
#TR=fread("TR.csv", colClasses = c(airconditioningtypeid = 'factor', architecturalstyletypeid = 'factor', buildingclasstypeid='factor', buildingqualitytypeid='factor', fireplaceflag='character'))

#IDA
coltypes = lapply(TR, class)
non_char = coltypes[coltypes != "character" & coltypes!="factor"]
#a couple more factor col
non_char = names(non_char)
IDA = dTail(non_char, TR)
IDA = IDA[order(IDA$pctNA)]

save(IDA, file ="ida.RData")

fillcols = c("poolcnt", "poolsizesum")
invisible(lapply(fillcols, function(x) TR[is.na(get(x)), (x):=0]))



#load("ida.RData")

idaplot = ggplot(data=IDA, aes(x=reorder(var, pctNA), y=pctNA))+geom_bar(stat = 'identity') + coord_flip()+theme_bw()
ggsave(idaplot, file=paste0(figdir, '/missing.png'))
ggplotly(idaplot)

#missing cols
micols = IDA[IDA$pctNA>=75]$var

#corr
#coltypes
coltypes = lapply(TR, class)

corFeatures = grep("numeric|integer", coltypes, value=T)
corFeatures = setdiff(names(corFeatures), "parcelid")

#remove missing cols
corFeatures = setdiff(corFeatures,micols)
#remove flag cols 
corFeatures = setdiff(corFeatures, flagids)
#remove useless cols
corFeatures = setdiff(corFeatures, uselesscols)
#remove year
corFeatures = setdiff(corFeatures, "assessmentyear")

cordf = as.data.frame(TR[, eval(corFeatures), with=F])

png("correlation.png")
corrplot(hetcor(cordf, use='complete.obs')$correlations, type = 'lower')
dev.off()

#abs error
TR[,abs_logerror:=abs(logerror)]

fwrite(TR, file="TR.csv")

TR=fread("TR.csv")
load("ida.RData")


setkey(TR, abs_logerror)
top5000 = TR[, head(.SD, 5000)]
bottom5000 = TR[,tail(.SD, 5000)]
top5000[,type:='best']
bottom5000[,type:='worst']
contrastdf = rbind(top5000, bottom5000)


for (icol in colnames(TR)){
  if(class(TR[,get(icol)])=="integer" | class(TR[,get(icol)])=="factor"){
    g = ggplot(data = contrastdf)+geom_histogram(aes_string(x=eval(icol), fill="type"), stat='count', position = 'dodge')
  }else{
    g = ggplot(data = contrastdf, aes_string(x=eval(icol), fill="type"))+geom_density(alpha=0.5)
  }
  ggsave(g, file=paste0(figdir, '/', icol, '_best_worst.png'))
}


for (icol in colnames(TR)){
  print(icol)
}


contractdf %>% ggplot(aes(x=latitude, fill=type))+geom_density(alpha=0.5)
