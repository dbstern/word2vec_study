## dependencias ----
source(file.path("R","dependencies.R"))

## load data ----
y <- read.table(file = file.path("input","labels_clean.txt"),
                header = FALSE, sep = "\t", stringsAsFactors = FALSE)[[1]]
bow <- readRDS(file.path("output","bow.rds")) # carregar bow
train <- gen_train(size = nrow(bow))

## fit models ----
bow <- bow %>% as(.,"sparseMatrix")
file <- file.path("output","fit_bow.rds")
model_bow <- model_list(
  x = bow, y = y, train = train, file_out = file, seed = 1,
  no_cores = no_cores, description = "bow")
rm(file)

bow_weights <- bow / rowSums(bow)

w2v100 <- read.vectors(file.path("output","w2v100.bin"))
w2v100 <- w2v100[colnames(bow_weights),]
w2v100 <- (bow_weights %*% w2v100) %>% as.matrix()
file <- file.path("output","fit_w2v100.rds")
model_w2v100 <- model_list(
  x = w2v100, y = y, train = train, file_out = file, seed = 10,
  no_cores = no_cores, description = "w2v100")
rm(file, w2v100)

w2v300 <- read.vectors(file.path("output","w2v300.bin"))
w2v300 <- w2v300[colnames(bow_weights),]
w2v300 <- (bow_weights %*% w2v300) %>% as.matrix()
file <- file.path("output", "fit_w2v300.rds")
model_w2v300 <- model_list(
  x = w2v300, y = y, train = train, file_out = file, seed = 1,
  no_cores = no_cores, description = "w2v300")
rm(file, w2v300)


# plots
# load and tidy data
fits <- paste0(c("fit_bow","fit_w2v100","fit_w2v300"),".rds")
fits <- file.path("output",fits)
x <- lapply(fits, function(fit) {
  fit <- readRDS(fit)
  roc <- NULL; auc <- NULL
  
  if(!is.null(fit$lasso$roc)) {
    lasso <- fit$lasso$roc[c("sensitivities","specificities")] %>%
      do.call(cbind, .) %>% as.data.frame() %>%
      mutate(model = "Lasso")
    roc <- rbind(roc,lasso)
    
    lasso <- data.frame(auc = fit$lasso$roc$auc[[1]]) %>%
      mutate(model = "Lasso")
    auc <- rbind(auc,lasso)
  }
  if(!is.null(fit$bst$roc)) {
    bst <- fit$bst$roc[c("sensitivities","specificities")] %>%
      do.call(cbind, .) %>% as.data.frame() %>%
      mutate(model = "XGboost")
    roc <- rbind(roc,bst)
    
    bst <- data.frame(auc = fit$lasso$roc$auc[[1]]) %>%
      mutate(model = "XGboost")
    auc <- rbind(auc,bst)
  }
  
  roc <- roc %>% mutate(npl = fit$description)
  auc <- auc %>% mutate(npl = fit$description)
  list(roc = roc, auc = auc)
})
roc <- lapply(x, function(y) y$roc) %>% do.call(rbind, .)
auc <- lapply(x, function(y) y$auc) %>% do.call(rbind, .)

ggplot(data = roc, aes(specificities,sensitivities, col = npl, linetype = model)) +
  geom_line() + labs(x = "Especificidade", y = "Sensibilidade") +
  scale_color_discrete(name = "NPL") +
  scale_linetype_discrete(name = element_blank())
ggsave2(filename = "roc", path = file.path("plots"), device = "pdf")

AUC = round(c(model_bow$lasso$roc$auc, model_bow$bst$roc$auc,
              model_w2v100$lasso$roc$auc, model_w2v100$bst$roc$auc,
              model_w2v300$lasso$roc$auc, model_w2v300$bst$roc$auc), 2)
tab = data.frame(Construção = c("Bag-of-words", "Word2vec (D = 100)", "Word2vec (D = 300)"),
                 "Lasso AUC" = AUC[c(1,3,5)], "XGBoost AUC" = AUC[c(2,4,6)])
xtable(tab, caption = "Comparação dos modelos pela área abaixo da curva ROC")
