require(bit64)
require(pROC)
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
require(missForest)
require(VIM)
require(mice)
require(caret)



wd="~/kaggle/zillow"
setwd(wd)
figdir = paste0(wd, '/figures')
source("/home/kyang/kaggle/RHelper/code/functions.R")


######################################
#preprocessing properties data input, will be needed to preprocess scoring data
######################################

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

######################################
# transformations/cleaning up
######################################

#####################
#taking care of flags
flagids = grep("flag", colnames(properties), value = T)
#check condition
properties[,hottubflag:=hottubflag=="true"]
properties[,fireplaceflag:=fireplaceflag=="true"]
properties[,taxdelinquencyflag:= taxdelinquencyflag=='Y']
#to 0, 1 integer
properties[, (flagids):=lapply(.SD, as.integer), .SDcols=flagids]
#####################

#####################
#Easy imputation, fill na with 0 for some columns. These columns have been individually inspected
#fill na with 0 selectively, because some columns should be left as NA and it should not create any problem for GBM but may for other methods
tofill = c("pooltypeid10", "pooltypeid2", "pooltypeid7", 'poolsizesum', 'poolcnt', 'architecturalstyletypeid', 'airconditioningtypeid', 'basementsqft', 'decktypeid', 'fireplacecnt', 'fullbathcnt', 'storytypeid', 'threequarterbathcnt', 'typeconstructiontypeid')
properties = create_mi(properties, cols = tofill)
invisible(lapply(tofill, function(x) properties[is.na(get(x)), (x):=0]))
#####################

#####################
#id cols to factors
#greedy but works for now
idcols = grep('id|code|desc', colnames(properties), value = T)
#remove _mi cols that are just created. they should remain as int
mi_cols = grep('_mi$',colnames(properties), value = T)
factor_cols = setdiff(idcols, mi_cols)
#keep parcelid in int
factor_cols = setdiff(factor_cols, 'parcelid')
properties[,(factor_cols):= lapply(.SD, as.factor), .SDcols = factor_cols]
#####################

#####################
#rawcensustractandblock vs censustractandblock
properties[,rawcensustractandblock:=gsub("\\.", "",rawcensustractandblock)]

#after pading, they are the same, except that censustractandblock has some "". replace censustractandblock with rawcensustractandblock and remove censustractandblock
properties[,rawcensustractandblock:=str_pad(rawcensustractandblock, 15, side="right", pad='0')]
properties[,censustractandblock:=rawcensustractandblock]
properties[,rawcensustractandblock:=NULL]

#####################

#####################
#tax delinquency year problem, only the last two digits are recorded, add 1900 or 2000 depending on the year
properties[!is.na(taxdelinquencyyear) & taxdelinquencyyear>=50, taxdelinquencyyear:=1900+taxdelinquencyyear]
properties[!is.na(taxdelinquencyyear) & taxdelinquencyyear>=50, taxdelinquencyyear:=2000+taxdelinquencyyear]
#####################



#####################
#read in transaction data and merge 
transactions = fread("data/train_2016.csv")
transactions <- transactions %>% rename(
  date = transactiondate
)
transactions[, dt:=ymd(date)]
transactions[, `:=`(month=lubridate::month(dt))]
#transactions[, dt:=NULL]
colnames(transactions)
#####################

#####################
#merge response
TR.all = merge(x=properties, y=transactions, by = 'parcelid', all.x=TRUE)
#fwrite(TR.all, file='TR.all.csv')
TR = TR.all[is.na(logerror)==F]
#fwrite(TR, file='TR.csv')

#####################
#some simple imputation that can benefit all modeling methods
#####################
#missing value imputation, this happens AFTER viewing IDA, but it stays here so I can preprocess scoring data too
#fix garage info, some garagecnt is not 0 but garagetotalsqft is, fix using lm
garagelm = lm(garagetotalsqft ~ garagecarcnt, data = TR[, list(garagecarcnt, garagetotalsqft)])
garagetotalsqft.predict = as.integer(predict(garagelm, newdata = TR[(garagetotalsqft==0 & garagecarcnt!=0)]))
TR[(garagetotalsqft==0 & garagecarcnt!=0), garagetotalsqft := garagetotalsqft.predict]
#set the rest nas into 0s for garage
TR[is.na(garagecarcnt), garagecarcnt:=0]
TR[is.na(garagetotalsqft), garagetotalsqft:=0]
######################

save(TR.all, TR, file='TR.rData')
#####################

#????????????????????
#to do 
#add unique id for TR
#????????????????????

#####################
#sel some cols for modeling based on IDA
pcNAs = TR[, list(name = colnames(.SD), class=lapply(.SD, class), pcNA=100*colMeans(is.na(.SD)))]
pcNAs[pcNA<30, name]
attributesToInclude = c('airconditioningtypeid','architecturalstyletypeid','basementsqft','bathroomcnt','bedroomcnt','calcbathcnt','decktypeid','area_total_calc','area_live_finished','fipsid','fireplacecnt','fullbathcnt','garagecarcnt','garagetotalsqft','hottubflag','lotsizesquarefeet','poolcnt','poolsizesum','pooltypeid10','pooltypeid2','pooltypeid7','propertycountylandusecode','propertylandusetypeid','propertyzoningdesc','regionidcity','regionidcounty','regionidzip','roomcnt','storytypeid','threequarterbathcnt','typeconstructiontypeid','build_year','fireplaceflag','structuretaxvaluedollarcnt','taxvaluedollarcnt','landtaxvaluedollarcnt','taxamount','taxdelinquencyflag','pooltypeid10_mi','pooltypeid2_mi','pooltypeid7_mi','poolsizesum_mi','poolcnt_mi','architecturalstyletypeid_mi','airconditioningtypeid_mi','basementsqft_mi','decktypeid_mi','fireplacecnt_mi','fullbathcnt_mi','storytypeid_mi','threequarterbathcnt_mi','typeconstructiontypeid_mi', 'month', 'logerror')
TR.s = TR[, attributesToInclude, with=F]

#####################
#remove high dimension factors
TR.factor = TR.s[, sapply(TR.s, is.factor), with=F]
levelCount = sapply(TR.factor, function(x)length(levels(x)))
toremove = names(levelCount[levelCount>=50])
TR.s[,(toremove):=NULL]
readyForH2o = copy(TR.s)
save(readyForH2o,file="forH2o.rData")
#####################



#for Caret and other lm models
#####################
#dummy variable
dummies <- dummyVars(logerror ~ ., data =TR.s)
TR.s.dummies = predict(dummies, newdata = TR.s)

#####################
#remove near 0 variance
nzv <- nearZeroVar(TR.s.dummies, freqCut = 95/5, uniqueCut = 5, saveMetrics = T)
nzv[nzv$nzv,][1:10,]
afterNZV = TR.s.dummies[,!nzv$nzv]
#####################

#####################
#impute NA before cor
preObj = preProcess(afterNZV, 
                    method = "medianImpute"   # or *bagImpute* / *knnImpute*
)
imputedData = predict(preObj, afterNZV)


#####################
# highly correlated
descrCor = cor(imputedData)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
filteredDescr <- imputedData[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
#####################

#####################
#linear Dependencies
comboInfo <- findLinearCombos(filteredDescr)
#nothing to remove
####################

#add back logerror and save
filteredDescr = cbind(filteredDescr, logerror = TR.s[,logerror])
save(filteredDescr, file = "afterPreprocess.rData")



#keep logerror to the non impute dt also to prevent leaking
#toremove = c(names(toremove), 'logerror')
#TR.s2 = TR.s[complete.cases(TR.s[,toremove, with=F])]

