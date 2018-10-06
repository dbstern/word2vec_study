library(futile.logger)
flog.appender(appender.tee("logger.log"))
flog.threshold(INFO)

library(tidyverse)
library(data.table)
library(xtable)
library(fBasics)

library(doParallel)
library(cluster)
no_cores <- max(1, detectCores()-1)

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
source(file.path('R','functions-load.R'))

ggsave2 <- function(filename, path, device="png", units="in", width=9, height=5.5) {
  filename <- paste0(filename,".",device)
  if(!dir.exists(path)) dir.create(path, recursive = T)
  ggsave(filename = filename, path = path,
         units = units, width = width, height = height)
}
namefile <- function(path, str, extension) {
  file <- format(Sys.time(), "%s") %>%
    paste0(str,"_", ., extension) %>%
    file.path(path, .)
  file <- lapply(seq(length(file)), function(i) {
    if(!file.exists(file[i]))
      return(file[i])
    namefile(path[i], str[i], extension[i])
  }) %>% unlist()
  return(file)
}

# ?
c("output","plots") %>% file.path(".", .) %>%
  lapply(., function(x) if(!dir.exists(x))dir.create(x, recursive = T))
