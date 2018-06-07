library(futile.logger)
flog.appender(appender.tee("logger.log"))
flog.threshold(INFO)

library(tidyverse)
# library(stringr)
library(data.table)
library(xtable)

library(doParallel)
library(cluster)
no_cores <- max(1, detectCores() - 6)

# matrizes esparcas
library(Matrix)
library(glmnet)

library(wordVectors)
library(fpc)
library(tm)

library(pROC)
library(MASS)
library(SparseM)
library(quanteda)
library(SnowballC)
library(textreadr)
library(striprtf)
library(pander)
library(grid)

library(xgboost)
library(Ckmeans.1d.dp) # used in plots of xgboost

source(file.path('R','functions-fit.R'))
