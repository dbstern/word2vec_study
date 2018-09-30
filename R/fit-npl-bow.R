## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
path_out <- file.path(".","output","bow")

## create bow ----
# matriz de contagem:  texto vs (palavra) 
file <- file.path(path_out,"bow.rds")
if(!file.exists(file)) {
  flog.info("Processar BOW")
  data <- file.path("input","reviews_clean.txt") %>%
    read.table(., header = F, sep = "\t", stringsAsFactors = F)
  bow <- dfm(data$V1, tolower = FALSE, verbose = TRUE)
  flog.info("Dimensao bow pre-exclusao palavras raras:", dim(bow))
  
  # exclusao de colunas cuja contagem global é menor que 1000
  bow <- bow[, colSums(bow) > 999]
  flog.info("Dimensao bow pos-exclusao palavras raras:", dim(bow))
  
  # contagem de espacos
  countspace <- numeric(length(data))
  for(i in 1:length(data))
    countspace[i] <- length(gregexpr(" ", data$V1[i])[[1]])
  bow <- cbind(countspace, bow)
  colnames(bow)[1] <- "</s>"
  
  rm(data,countspace,i)
  saveRDS(bow, file = file)
  
  case <- data.frame(cluster = NA, method = NA, k = NA, file_out = file)
  dirname(path_out) %>%
    file.path(.,"track_bow.txt") %>%
    write.table(x = case, file = ., append = file.exists(.),
                col.names = !file.exists(.), row.names = F)
} 
rm(file)

