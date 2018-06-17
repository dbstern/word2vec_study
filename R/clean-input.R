## dependencias ----
source(file.path('.','R','dependencies.R'))

## criar arquivo das reviews limpas ----
file <- file.path("input","reviews_clean.txt")
if(!file.exists(file)) {
  data <- fread(file.path("input","Reviews.csv"), header=T)
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

file <- file.path("input", "labels_clean.txt")
if(!file.exists(file)) {
  data <- fread(file.path("input","Reviews.csv"), header=T)
  y <- as.numeric(data$Score == 5)
  y <- y[-233939] #Texto em branco 
  write.table(y,file,row.names = F,col.names = F)
  rm(data,file)
}

