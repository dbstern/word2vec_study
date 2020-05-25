## source global variables ----
source(file.path('scripts','_global.R'))

## load packages ----
requiredPackages <- c(
  "futile.logger",
  "textdata", # imdb dataset
  "data.table", # fread
  "tidyverse",
  "doParallel",
  "fBasics", # functions on columns of matrices
  "tm", # text mining package
  "quanteda", # bow
  "wordVectors", # w2v
  "Rtsne",
  "bigmemory", # memory-mapped files
  "biglasso", # lasso using bigmemory object
  "bigstatsr", # help functions for bigmemory
  "pROC",
  "xgboost"
)
for(pkg in requiredPackages){
  if(!nzchar(system.file(package = pkg))) {
    if(pkg == "wordVectors") {
      if(!nzchar(system.file(package = "devtools")))
        install.packages("devtools")
      devtools::install_github("bmschmidt/wordVectors")
    } else {
      install.packages(pkg,character.only = TRUE)
    }
  }
  library(pkg,character.only = TRUE)
}

# futile.logger
logpath <- file.path(basepath,sprintf("logger_%s.log", db))
futile.logger::flog.appender(futile.logger::appender.tee(logpath))
futile.logger::flog.threshold(INFO)

# library(textdata) # imdb dataset

# library(data.table) # fread
# library(tidyverse)
# library(doParallel)
no_cores <- max(1, parallel::detectCores()-1)

# library(fBasics) # functions on columns of matrices
# library(tm) # text mining package
# library(quanteda) # bow
# library(wordVectors)
# library(Rtsne)

# library(bigmemory) # memory-mapped files
options(bigmemory.allow.dimnames=TRUE)
# library(biglasso) # lasso using bigmemory object
# library(bigstatsr) # help functions for bigmemory
# library(pROC)
# library(xgboost)

## help functions ----
ggsave2 <- function(filename, path, device="png", units="in", width=9, height=5.5) {
  filename <- paste0(filename,".",device)
  if(!dir.exists(path)) dir.create(path, recursive = T)
  ggsave(filename = filename, path = path,
         units = units, width = width, height = height)
}

## source function files ----
source(file.path('scripts','0_functions-fit.R'))
source(file.path('scripts','0_functions-load.R'))

## list sample cases ----
file <- get_relpath("sample",NULL)
cases <- expand.grid(
  sample_seed = 1,
  sample_igeq = c(4,5),
  sample_prob = c(.5,.0088)
)
if(stringr::str_detect(db,"[.]")) {
  cases <- data.frame(sample_id = 1)
} else if(db != "amzn") {
  cases <- dplyr::mutate(cases, sample_igeq = NULL) %>% unique(.)
}
if(file.exists(file)) {
  cases_rm <- file %>%
    read.table(., header = T, stringsAsFactors = F) %>%
    dplyr::mutate(rm = T)
  cases <- merge(cases,cases_rm, all = T) %>%
    dplyr::filter(is.na(rm)) %>%
    dplyr::select(-rm)
  id <- max(cases_rm$sample_id)
} else {
  id <- as.integer(0)
}
cases <- cases %>% mutate(sample_id = row_number() + id) %>%
  dplyr::select(., sample_id,everything())
if(!dir.exists(dirname(file))) dir.create(dirname(file), recursive = T)
write.table(x = cases, file = file, append = file.exists(file),
            col.names = !file.exists(file), row.names = F)
rm(cases,cases_rm,id,file)

##  ----
