gen_train <- function(size, seed = 1234, prob = .5) {
  set.seed(seed)
  train = sample(c(T,F), size, c(prob,1-prob), replace = TRUE)
  return(train)
}

bow_xgweight <- function(model, bow, w2v, indic_bow = T, norm_bow = T, norm_weigh = T) {
  xgimportance <- xgb.importance(
    colnames(bow), model  = model)
  
  words <- xgimportance$Feature
  gain <- xgimportance$Gain %>%
    diag(., nrow = length(.)) %>% as(.,"sparseMatrix")
  bow <- bow[,words]
  
  if(indic_bow) bow <- (bow > 0)
  if(norm_bow) bow <- bow / rowSums(bow)
  
  wxgb <- bow %*% gain
  if(norm_weigh) wxgb <- wxgb * (rowSums((bow > 0) %*% gain) ^ (-1))
  if(!is.null(w2v)) wxgb <- wxgb %*% w2v[words,]
  return(wxgb)
}

fit_lasso <- function(x, y, train, file_out, description=NA, seed = NULL, with_grid = F) {
  flog.info("START: Ajuste lasso%s - %s - seed %s",
            ifelse(with_grid, " com grid", ""), description, seed)
  
  fit <- list()
  if(file.exists(file_out)) {
    flog.info("Carregando %s", file_out)
    fit <- readRDS(file_out)
  }
  
  xtrain <- x[train,]; ytrain <- y[train]
  xtest <- x[!train,]; ytest <- y[!train]
  
  if(!"lasso" %in% names(fit)) {
    flog.info(paste0("Estimando Lasso (seed - ",seed,")"))
    lambda <- if(with_grid) c(0.5^(2:20),0) else NULL
    set.seed(seed = seed)
    ftry({
      fit$lasso <- cv.glmnet(
        x = xtrain, y = ytrain, family = "binomial", type.measure = "class",
        standardize = FALSE, lambda = lambda)
      saveRDS(fit, file = file_out)
    })
  }
  if(!"test" %in% names(fit)) {
    if(is.null(fit$lasso)) return(fit)
    ftry({
      fit$test <- as.numeric(predict(
        fit$lasso, xtest, type = "response", s = "lambda.min"))
      saveRDS(fit, file = file_out)
    })
  }
  if(!"roc" %in% names(fit)) {
    flog.info("ROC Lasso")
    ftry({
      fit$roc <- roc(ytest, fit$test)
      saveRDS(fit, file = file_out)
    })
  }
  flog.info("END")
  return(fit)
}

fit_xgboost <- function(x, y, train, file_out, description=NA, seed = NULL, no_cores = 1) {
  flog.info("START: Ajuste xgboost - %s - seed %s", description, seed)
  
  fit <- list()
  if(file.exists(file_out)) {
    flog.info("Carregando %s", file_out)
    fit <- readRDS(file_out)
  }
  
  xtrain <- x[train,]; ytrain <- y[train]
  xtest <- x[!train,]; ytest <- y[!train]
  
  if(!"xgbcv" %in% names(fit)) {
    flog.info(paste0("xgb.cv (seed - ",seed,")"))
    set.seed(seed = seed)
    ftry({
      fit$xgbcv <- xgb.cv(
        data = xtrain, label = ytrain, nthread = no_cores, nround = 10000,
        nfold = 2, metrics = "error", objective = "binary:logistic",
        lambda = 0.01, verbose = FALSE)
      saveRDS(fit, file = file_out)
    })
  }
  if(!"xgb" %in% names(fit)) {
    flog.info(paste0("xgboost (seed - ",seed,")"))
    nround <- which.min(fit$xgbcv$evaluation_log$test_error_mean)
    set.seed(seed = seed)
    ftry({
      fit$xgb <- xgboost(
        data = xtrain, label = ytrain, nthread = no_cores, nround = nround,
        objective = "binary:logistic", lambda = 0.01, verbose = FALSE)
      saveRDS(fit, file = file_out)
    })
  }
  if(!"test" %in% names(fit)) {
    fit$test <- as.numeric(predict(fit$xgb, xtest, type = "response"))
    saveRDS(fit, file = file_out)
  }
  if(!"roc" %in% names(fit)) {
    flog.info("ROC xgboost")
    fit$roc <- roc(ytest, fit$test)
    saveRDS(fit, file = file_out)
  }
  flog.info("END")
  return(fit)
}
