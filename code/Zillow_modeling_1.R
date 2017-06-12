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
modeldir = paste0(wd, '/models')
source("/home/kyang/kaggle/RHelper/code/functions.R")

load("afterPreprocess.rData")
set.seed(1)
##############
#caret, first try
#gated for fast speed

ycol = 'logerror'
gate = 0.1

alldata = data.table(filteredDescr)
alldata = alldata[sample(.N, as.integer(gate*.N))]

#alldata_y = copy(alldata[, ycol, with=F])
#alldata_x = copy(alldata[, -ycol, with=F])

#split data
inTrain = createDataPartition(y = alldata[,logerror], p=0.75, list=FALSE, seed=1)
training <- alldata[inTrain,]
testing <- alldata[-inTrain,]

qplot(training[,logerror])


##########################
#not sure if we should do this
#center and scale
#watch for transforming target variable
preObj <- preProcess(training[, -ycol, with=F], method=c("center","scale"))
trainTransformed = predict(preObj, training)
testTransformed = predict(preObj, testing)

#box-cox
preObj <- preProcess(trainTransformed[, -ycol, with=F], method=c("BoxCox"))
trainTransformed = predict(preObj, trainTransformed)
testTransformed = predict(preObj, testTransformed)
##########################

#########################
#modeling

formula = as.formula(paste(ycol, '~', '.', collapse = " "))

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 2)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:3)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit1 <- train(formula , data = training, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 trControl = fitControl,
                 #tuneGrid = gbmGrid,
                 verbose = FALSE)
gbmFit1

save(gbmFit1, file='caret_gbm.rData')

#vi
varImp(gbmFit1)

#tree depth
plot(gbmFit1)

#ggplot
g = ggplot(gbmFit1)
g

#predict test
test_predict = predict(gbmFit1, newdata=testing)

metric = data.frame(predict = test_predict, real = testing[,logerror], diff = testing[,logerror]-test_predict)
ggplot(data=metric)+geom_density(aes(x=diff))


glmFit1 <- glm(formula , data = training)


#logistic regression
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(2014)

glmBoostModel <- train(Class ~ ., data=trainData, method = "glmboost", metric="ROC", trControl = fitControl, tuneLength=5, center=TRUE, family=Binomial(link = c("logit")))


