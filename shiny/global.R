# this should be the same as the one set at 'scripts/_global.R', 
# unless you move the files to another directory
outpath <- "parent_directory_where_files_are_saved"
rmstopw <- FALSE

## load packages ----
library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
# library(shinyjs)

library(tidyverse)
library(plotly)
library(xtable)

library(biglasso) # coef()

## source function files ----
file.path("..","scripts") %>%
  dir(path = ., pattern = "functions", full.names = T) %>%
  purrr::map(~ source(.))


## button conf ----
cases_opts <- read.csv(file.path(".","btns.csv"), stringsAsFactors = F)

## ----
gg_color_hue <- function(vcol) {
  # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  n <- length(levels(vcol))
  hues <- scales::hue_pal()(n)
  names(hues) <- levels(vcol)
  return(hues)
  # return(hues[vcol])
}

selectInput2 <- function(dbl,multiple,cases) {
  id <- paste0("select_",dbl$opt,if(dbl$type=="d2v")"d2v")
  track <- ifelse(dbl$type=="model","mw2v",dbl$type)
  choices <- if(dbl$type=="model" & dbl$opt=="stat") {
    levels(cases[[track]][[dbl$opt]])[-1]
  } else {
    sort(unique(cases[[track]][[dbl$opt]]))
  }
  
  tagList(
    selectInput(
      id, label = dbl$label, multiple = multiple,
      choices = choices, selected = dbl$selected),
    bsTooltip(id, dbl$info, placement = "top")
  )
}