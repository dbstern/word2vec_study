gen_train <- function(size, seed = 1234, prob = .5) {
  set.seed(seed)
  train = sample(c(T,F), size, c(prob,1-prob), replace = TRUE)
  return(train)
}

bow_xgweight <- function(model, bow, w2v) {
  xgimportance <- xgb.importance(
    colnames(bow), model  = model)
  
  words <- xgimportance$Feature
  gain <- xgimportance$Gain %>%
    diag(., nrow = length(.)) %>% as(.,"sparseMatrix")
  
  w2v_bow_wxgb <- bow[,words] %*% gain
  if(!is.null(w2v))
    w2v_bow_wxgb <- w2v_bow_wxgb %*% w2v[words,]
  return(w2v_bow_wxgb)
}

model_lasso <- function(x, y, train, file_out, seed = NULL) {
  model <- list()
  if(file.exists(file_out)) {
    flog.info("Carregando %s", file_out)
    model <- readRDS(file_out)
  }
  
  xtrain <- x[train,]; ytrain <- y[train]
  xtest <- x[!train,]; ytest <- y[!train]
  
  flog.info("Estimando Lasso")
  if(!is.null(seed)) set.seed(seed)
  ftry({
    model$lasso <- cv.glmnet(
      x = xtrain, y = ytrain, family = "binomial", type.measure = "class",
      standardize = FALSE, lambda = c(0.5^(2:20),0))
  })
  saveRDS(model, file = file_out)
  if(is.null(model$lasso)) return(model)
  
  ftry({
    model$lasso$test <- as.numeric(predict(
      model$lasso, xtest, type = "response", s = "lambda.min"))
  })
  saveRDS(model, file = file_out)
  if(is.null(model$lasso$test)) return(model)
  
  flog.info("ROC Lasso")
  ftry({
    model$lasso$roc <- roc(ytest, model$lasso$test)
  })
  saveRDS(model, file = file_out)
  return(model)
}

model_xgboost <- function(x, y, train, file_out, no_cores = 1) {
  model <- list()
  if(file.exists(file_out)) {
    flog.info("Carregando %s", file_out)
    model <- readRDS(file_out)
  }
  
  xtrain <- x[train,]; ytrain <- y[train]
  xtest <- x[!train,]; ytest <- y[!train]
  
  flog.info("Estimando bst(CV)")
  ftry({
    model$bstCV <- xgb.cv(
      data = xtrain, label = ytrain, nthread = no_cores, nround = 10000,
      nfold = 2, metrics = "error", objective = "binary:logistic",
      lambda = 0.01, verbose = FALSE)
  })
  saveRDS(model, file = file_out)
  
  flog.info("Estimando bst")
  nround <- which.min(model$bstCV$evaluation_log$test_error_mean)
  ftry({
    model$bst <- xgboost(
      data = xtrain, label = ytrain, nthread = no_cores, nround = nround,
      objective = "binary:logistic", lambda = 0.01, verbose = FALSE)
  })
  saveRDS(model, file = file_out)
  ftry({
    model$bst$test <- as.numeric(predict(model$bst, xtest, type = "response"))
  })
  saveRDS(model, file = file_out)
  
  flog.info("ROC bst")
  ftry({
    model$bst$roc <- roc(ytest, model$bst$test)
  })
  saveRDS(model, file = file_out)
  return(model)
}

model_list <- function(x, y, train, file_out, seed = NULL, no_cores = 1, description) {
  model <- list(description = description)
  if(file.exists(file_out))
    model <- readRDS(file_out)
  saveRDS(model, file = file_out)
  
  # evitar repetir calculos
  # talvez adicionar um parametro para saber se deseja sobrescrever fits anteriores
  if(!"lasso" %in% names(model))
    model <- model_lasso(
      x = x, y = y, train = train, file_out = file_out, seed = seed)
  if(!"bst" %in% names(model))
    model <- model_xgboost(
      x = x, y = y, train = train, file_out = file_out, no_cores = no_cores)
  return(model)
}
