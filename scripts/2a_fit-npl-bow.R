# proc - bow ficou diferente devido a mudancas que fiz no tratamento inicial

## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## create bow ----
# matriz de contagem:  texto vs (palavra) 
file <- get_relpath(type = "bow", T)
if(!file.exists(file)) {
  flog.info("Processar BOW")
  data <- load_txt()
  bow <- dfm(data, tolower = FALSE, verbose = TRUE)
  flog.info("Dimensao bow pre-exclusao palavras raras:", dim(bow))
  
  # exclusao de colunas cuja contagem global é menor que 1000
  bow <- bow[, colSums(bow) > 999]
  flog.info("Dimensao bow pos-exclusao palavras raras:", dim(bow))
  
  # contagem de espacos
  countspace <- numeric(length(data))
  for(i in 1:length(data))
    countspace[i] <- length(gregexpr(" ", data[i])[[1]])
  bow <- cbind(countspace, bow)
  colnames(bow)[1] <- "</s>"
  
  rm(data,countspace,i)
  saveRDS(bow, file = file)

  case <- data.frame(rmstopw = rmstopw, k = NA, file_clw2v = NA, file_bow = basename(file))
  get_relpath("bow",NULL) %>%
    write.table(x = case, file = ., append = file.exists(.),
                col.names = !file.exists(.), row.names = F)
  rm(bow,case)
}
rm(file, newfile)

