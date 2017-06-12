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
require(mice)
require(missForest)


wd="~/kaggle/zillow"
setwd(wd)
figdir = paste0(wd, '/figures')
source("/home/kyang/kaggle/RHelper/code/functions.R")

load("TR.rData")

uselesscols = c('longitude', 'latitude')

######################################
# IDA
######################################

coltypes = lapply(TR, class)
coltypes %in% c("character", "factor", "Date")
non_char = coltypes[! coltypes %in% c("character", "factor", "Date")]
#a couple more factor col
non_char = names(non_char)
IDA = dTail(non_char, TR)
IDA = IDA[order(IDA$pctNA)]

save(IDA, file ="ida.RData")

#######################
#IDA plot

#load("ida.RData")
idaplot = ggplot(data=IDA, aes(x=reorder(var, pctNA), y=pctNA))+geom_bar(stat = 'identity') + coord_flip()+theme_bw()
ggsave(idaplot, file=paste0(figdir, '/missing.png'))
ggplotly(idaplot)

#excessive missing cols
micols = IDA[IDA$pctNA>=75]$var

#missing_indicator cols
mi_id_cols = grep('_mi$', colnames(TR), value = T)

#########################
#corelation plots between some variables and target, not very useful if you have other things.
#corr
#coltypes
coltypes = lapply(TR, class)

corFeatures = grep("numeric|integer", coltypes, value=T)
corFeatures = setdiff(names(corFeatures), "parcelid")

#remove excessive missing cols
corFeatures = setdiff(corFeatures,micols)
#remove missing indicator cols
#corFeatures = setdiff(corFeatures, mi_id_cols)
#remove flag cols 
corFeatures = setdiff(corFeatures, flagids)
#remove useless cols
corFeatures = setdiff(corFeatures, uselesscols)
#remove year
corFeatures = setdiff(corFeatures, "assessmentyear")

cordf = as.data.frame(TR[, eval(corFeatures), with=F])

png(paste0(figdir, "/correlation.png"))
corrplot(cor(cordf, use='complete.obs'), type = 'lower')
dev.off()

#################
#abs error
TR[,abs_logerror:=abs(logerror)]
saveRDS(TR, "TR_with_abslogerror.rds")

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

#error dist
ggsave(qplot(TR[,logerror], bins=50)+xlim(-1,1), file = paste0(figdir, '/logerror_histo.png'))
