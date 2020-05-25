## dependencias ----
source(file.path('.','scripts','0_dependencies.R'))

## download data - if necessary ----
download_db()

## load data ----
data <- load_raw_data()

## clean reviews and save ----
file <- get_relpath("txt",T)
if(!file.exists(file)) {
  txt <- data$txt %>%
    # lower case
    tolower() %>%
    # remove linebreaks
    gsub("(\n)|(<br />)", " ", .) %>%
    # remove punctuation + sub/superscripts
    gsub(., pattern = "[[:punct:]]", replacement = " ") %>%
    removeNumbers()
  
  # remove stopwords
  if(!(db %in% c("proc","ptin")) & rmstopw)
    txt <- removeWords(txt, stopwords("english"))
  txt <- txt %>% stripWhitespace(.)
  # remove single letter words
  if(rmstopw)
    txt <- txt %>% gsub(" [[:alnum:]] ", " ", .)
  # remove whitespace at the start/end
  txt <- txt %>% gsub("(^\\s+)|(\\s+$)", "", .)
  saveRDS(txt, file = file)
  rm(txt,file)
}

## save labels ----
file <- get_relpath("label",T)
if(!file.exists(file)) {
  if(db == "nltk.reuters") {
    dplyr::select(data, -c(train:txt)) %>%
      saveRDS(.,file)
  } else {
    saveRDS(data$label,file)
  }
  rm(file)
}

## if there is a predetermined train/test split ----
file <- get_relpath("train",T)
if(!file.exists(file) & ("train" %in% names(data))) {
  saveRDS(data$train,file)
  rm(file)
}

## ----
rm(data,file)

