## dependencias ----
source(file.path("R","dependencies.R"))

## load data ----
data <- fread(file.path("input","Reviews.csv"), header=T)
bow <- readRDS(file.path("output","bow.rds")) # carregar bow

train <- gen_train(size = nrow(bow))
y <- as.numeric(data$Score == 5)[-233939] #Texto em branco 
rm(data)

## fit models ----
bow <- bow %>% as(.,"sparseMatrix")
file <- file.path("output","fit_bow.rds")
model_bow <- model_list(
  x = bow, y = y, train = train, file_out = file, seed = 1,
  no_cores = no_cores, description = "bow")
rm(file)

w2v100 <- read.vectors(file.path("output","w2v100.bin"))
w2v100 <- w2v100[colnames(bow),]
w2v100 <- (bow %*% w2v100) %>% as.matrix()
file <- file.path("output","fit_w2v100.rds")
model_w2v100 <- model_list(
  x = w2v100, y = y, train = train, file_out = file, seed = 10,
  no_cores = no_cores, description = "w2v100")
rm(file, w2v100)

w2v300 <- read.vectors(file.path("output","w2v300.bin"))
w2v300 <- w2v300[colnames(bow),]
w2v300 <- (bow %*% w2v300) %>% as.matrix()
file <- file.path("output", "fit_w2v300.rds")
model_w2v300 <- model_list(
  x = w2v300, y = y, train = train, file_out = file, seed = 1,
  no_cores = no_cores, description = "w2v300")
rm(file, w2v300)
