load_labels <- function(indic_geq) {
  raw <- file.path("..","input","labels_raw.txt") %>%
    read.table(., header = F) %>% "[["(1)
  raw <- raw[-233939] # texto em branco
  if(is.null(indic_geq)) return(raw)
  raw >= indic_geq
}

