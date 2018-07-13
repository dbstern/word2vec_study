## dependencias ----
source(file.path('.','R','dependencies.R'))

## criar arquivo das reviews limpas ----
file <- file.path("input","reviews_clean.txt")
if(!file.exists(file)) {
  data <- fread(file.path("input","Reviews.csv"), header=T)$Text
  data <- data %>%
    gsub("<br />", " ", .) %>% # Retira linebreaks
    tolower()  %>% # Letras minusculas
    # gsub("[.,]"," ", x = .) %>% # substitui pontos por espacos para nao juntar palavras na remocao de pontos
    removePunctuation() %>% # Retira pontuação
    removeNumbers() %>% # Retura números
    removeWords(., stopwords("english")) %>% # Retira stopwords
    gsub(" [[:alnum:]] ", " ", .) %>% # Retira letras sozinhas
    gsub("[[:space:]]{1,}"," ", .) %>% # Retira espaços duplos
    gsub("^\\s+", "", .) %>% # Retira espaços do início
    gsub("\\s+$", "", .) # Retira espaços do final
  write(data, file)
  rm(data,file)
  # uma das linhas esta vazia, qdo o arq for lido com read.table essa
  # linha vazia vai ser pulada (blank.skip.lines=F evita isso)
}

files <- file.path("input", paste0("labels_",c("raw","clean"),".txt"))
if(!all(file.exists(files))) {
  x <- fread(file.path("input","Reviews.csv"), header = T)$Score
  write.table(x,files[1],row.names = F,col.names = F)
  
  x <- as.numeric(x == 5)[-233939] # texto em branco 
  write.table(x,files[2],row.names = F,col.names = F)
  rm(x,files)
}

