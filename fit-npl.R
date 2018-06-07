## Dependências ----
source(file.path("R","dependencies.R"))

## Criar bow ----
# matriz de contagem:  texto vs (palavra) 
file <- file.path("output","bow.rds")
if(!file.exists(file)) {
  data <- file.path("input","reviews_clean.txt") %>%
    read.table(., header = F, sep = "\t", stringsAsFactors = F)
  bow <- dfm(data$V1, tolower = FALSE, verbose = TRUE)
  
  # exclusao de colunas cuja contagem global é menor que 1000
  bow <- bow[, colSums(bow) > 999]
  
  # contagem de espacos
  countspace <- numeric(nrow(data))
  for(i in 1:nrow(data))
    countspace[i] <- length(gregexpr(" ", data$V1[i])[[1]])
  bow <- cbind(countspace, bow)
  colnames(bow)[1] <- "</s>"
  
  rm(data,countspace,i)
  saveRDS(bow, file = file)
} else {
  bow <- readRDS(file)
}
rm(file)

## Gerar os modelos word2vec ----
file_in <- file.path("input","reviews_clean.txt")

file_out <- file.path("output","w2v100.bin")
if(!file.exists(file_out)) {
  w2v100 <- train_word2vec(
    train_file = file_in, output_file = file_out,
    vectors = 100, threads = no_cores, window = 12,
    iter = 10, negative_samples = 0, min_count = 1000)
} else {
  w2v100 <- read.vectors(file_out)
}
w2v100 <- w2v100[colnames(BOW),]

file_out <- file.path("output","w2v300.bin")
if(!file.exists(file_out)) {
  w2v300 <- train_word2vec(
    train_file = file_in, output_file = file_out,
    vectors = 300, threads = no_cores, window = 12,
    iter = 10, negative_samples = 0, min_count = 1000)
} else {
  w2v300 <- read.vectors(file_out)
}
w2v300 <- w2v300[colnames(BOW),]
rm(file_in,file_out)

