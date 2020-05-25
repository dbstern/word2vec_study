## global variables! - this will define loading functions, etc ----

## '.' indicates that the dataset is going to downloaded from a package! - pck_name.db_name
# amzn - amazon food and wine reviews
# proc - dados processos abj
# nltk.reuters - Reuters-21578
# nok - rcv1 - Reuters Corpus Volume I (couldnt find cirpus, only tokenized texts)
# ptin - novos dados de 'proc', com as petições iniciais 
# imdb - db usado no artigo do d2v (mikolov)
db <- "nltk.reuters"

# remove stopwords/single letter words
rmstopw <- T

basepath <- file.path("/mnt/36D0BB68D0BB2CCD/dbstern", db)
if(!dir.exists(basepath)) dir.create(basepath, recursive = T)

