## dependencias ----
source(file.path("R","dependencies.R"))

## load data ----
data <- fread(file.path("input","Reviews.csv"), header=T)
bow <- readRDS(file.path("output","bow.rds")) # carregar bow

train <- gen_train(size = nrow(bow))
y <- as.numeric(data$Score == 5)[-233939] #Texto em branco 

## calculate weights ----
file <- file.path("output","weights.rds")
if(!file.exists(file)) {
  weights.list <- list(description = "weights")
  saveRDS(weights.list, file = file)
  weights.list <- model_xgboost(x = bow, y = y, train = train,
                                file_out = file, no_cores = no_cores)
} else {
  weights.list <- readRDS(file)
}

# w <- xgb.importance(colnames(bow), model  = weights.list$bst_bow)
# xgb.plot.importance(w, rel_to_first = TRUE, xlab = "Relative importance")
# xgb.ggplot.importance(importance_matrix = w, measure = "Gain", top_n = 100) +
#   scale_y_continuous(expand = c(0,0)) +
#   ggplot2::ylab("Gain")

## apply weights ----
w2v100 <- read.vectors(file.path("output","w2v100.bin"))
w2v100_bow_wxgb <- bow_xgweight(
  model = weights.list$bst_bow, bow = bow, w2v = w2v100) %>% as.matrix

file <- file.path("output","fit_w2v100_wxgb.rds")
model_w2v100_wxgb <- model_list(
  x = w2v100_bow_wxgb, y = y, train = train, file_out = file, seed = 1,
  no_cores = no_cores, description = "w2v100_wxgb")

