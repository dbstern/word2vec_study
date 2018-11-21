library(shiny)
library(shinydashboard)

library(tidyverse)
library(plotly)

library(Rtsne)
library(wordVectors)

file.path(".","R") %>%
  dir(path = ., pattern = "functions", full.names = T) %>%
  purrr::map(~ source(.))

cases <- dir(path = file.path("..","output"), pattern = "track_.*txt", full.names = T) %>% 
  setNames(basename(.) %>% stringi::stri_replace_all(str = ., regex = "(track_)|(.txt)","")) %>%
  purrr::map(., function(file) {
    type <- basename(file) %>%
      stringi::stri_replace_all(str = ., regex = "(track_)|(.txt)","")
    file %>% read.table(., header = T, stringsAsFactors = F) %>%
      dplyr::rename_all(., .funs = function(x) paste0(type,"_",x))
  })
