## global variables! - this will define loading functions, etc ----

# define which of the following datasets is going to be run:
# amzn - amazon food and wine reviews
# proc - dados processos abj
# nltk.reuters - Reuters-21578
# nok - rcv1 - Reuters Corpus Volume I (couldnt find cirpus, only tokenized texts)
# ptin - novos dados de 'proc', com as petições iniciais 
# imdb - db usado no artigo do d2v (mikolov)
db <- "nltk.reuters"

# define if stopwords/single letter words should be removed
rmstopw <- T

# directory where files will be saved 
basepath <- file.path("parent_directory_where_files_are_saved", db)
if(!dir.exists(basepath)) dir.create(basepath, recursive = T)

