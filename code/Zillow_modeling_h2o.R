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

load("forH2o.rData")
set.seed(1)

ycol = 'logerror'

#start h2o
h = h2o.init(nthread=7)

#input data
input = as.h2o(readyForH2o)

hyper_parameters <- list(
  max_depth=c(15),
  learn_rate=c(0.05),
  min_rows=100
)

#grid search criteria
search_criteria <- list(strategy="RandomDiscrete",
                        max_models=100,
                        stopping_rounds = 5,
                        stopping_tolerance = 1e-5,
                        stopping_metric = "RMSE"
)

#perform search
grid <- h2o.grid(algorithm="gbm", 
                 grid_id = ycol,
                 hyper_params = hyper_parameters,
                 search_criteria = search_criteria,
                 training_frame = input,
                 nfolds = 5,
                 y = ycol, x = setdiff(colnames(readyForH2o), c(ycol)),
                 score_tree_interval = 10,
                 keep_cross_validation_predictions=T,
                 seed=12345
)

h2oglm1 = h2o.glm(x=setdiff(colnames(readyForH2o), c(ycol)),
                  y=ycol,
                  training_frame = input,
                  nfolds = 5,
                  family = c("gaussian")
                  )

p = 1- pchisq(2421.398, 90810)
p
