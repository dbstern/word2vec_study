## dependencias ----
source(file.path("R","dependencies.R"))
library(ggdendro)

data <- fread(file.path("input","Reviews.csv"), header=T)
score <- as.numeric(data$Score)[-233939] #Texto em branco 
y <- as.numeric(data$Score == 5)[-233939] #Texto em branco 
bow <- readRDS(file.path("output","bow.rds")) # carregar bow
w2v100 <- read.vectors(file.path("output","w2v100.bin"))
w2v100 <- w2v100[colnames(bow),]
w2v300 <- read.vectors(file.path("output","w2v300.bin"))

## db descriptive analysis ----
# histograma dos scores
data %>%
  group_by(Score) %>%
  summarise(freq = n()/nrow(data)) %>%
  ggplot(., aes(x = Score, y = freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(freq,2), vjust = 1.3), col = "white") +
  labs(y = "Frequência Relativa")
ggsave(filename = "db_freq-score.pdf", path = "plots")


## bow analysis ----
# % de textos em que as palavras excluidas apareciam em media
999/nrow(bow)

# summary of freq of words by review
mySummary <- function(vector, na.rm = FALSE, round = 2){
  results <- c(summary(vector), 'Std. Dev' = round(sd(vector, na.rm), 2))
  return(results)
}
bind_rows(
  data.frame(total = unclass(rowSums(bow)), y = score) %>%
    group_by(y) %>%
    do(mySummary(.$total) %>% unclass() %>% t() %>% as.data.frame()) %>%
    ungroup() %>%
    mutate(y = as.character(y)),
  data.frame(total = unclass(rowSums(bow)), y = y) %>%
    group_by(y) %>%
    do(mySummary(.$total) %>% unclass() %>% t() %>% as.data.frame()) %>%
    ungroup() %>%
    mutate(y = paste("y =", y)),
  unclass(rowSums(bow)) %>% mySummary() %>% unclass() %>% 
    t() %>% as.data.frame() %>%
    mutate(y = "all")
) %>%
  xtable::xtable(
    caption = "Resumo da contagem de palavras por resenha."
  )

## w2v100 analysis ----
# similariry tables
w2v100 %>% closest_to(~ "good") %>%
  xtable::xtable(
    caption = "Palavras com representação vetorial mais próximas de 'good'") %>%
  print(include.rownames = F)

cbind(
  w2v100 %>% closest_to(~ "good"-"bad") %>% as.tibble(),
  w2v100 %>% closest_to(~ "good"+"bad") %>% as.tibble()
) %>%
  xtable::xtable(
    caption = "Palavras com representação vetorial mais próximas de 'good'-'bad' e 'good'+'bad'") %>%
  print(include.rownames = F)

# cluster analysis
path <- file.path("plots","w2v100_dendogram")
if(!dir.exists(path)) dir.create(path)
h <- 2
d <- cosineSimilarity(w2v100,w2v100)
# pelo que parece o ward.d deve receber d^2 e o ward.d2 d
hc <- hclust(d = as.dist(1-d), method = "ward.D2")
cuts <- cut(hc %>% as.dendrogram(), h = h)$lower
gg <- lapply((cuts %>% length %>% seq), function(x) {
  filename <- paste0("w2v100_dendogram_cut-",h,"_lower-",x,".pdf")
  str <- paste("Método Ward - Altura ", h, "Galho -", x)
  ggdendrogram(cuts[[x]]) + labs(title = str)
  ggsave(filename = filename, path = path)
})

