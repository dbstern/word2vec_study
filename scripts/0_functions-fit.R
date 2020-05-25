gen_train <- function(size, seed = 1234, prob = .5) {
  file <- get_relpath("train",T)
  if(file.exists(file)) {
    return(as.logical(readRDS(file = file)))
  }
  set.seed(seed)
  train = sample(c(T,F), size, c(prob,1-prob), replace = TRUE)
  return(train)
}

fit_lasso <- function(x, y, train, file_out, seed = NULL, ncores = 1) {
  flog.info("START: Ajuste lasso - seed %s", seed)
  
  fit <- list()
  if(file.exists(file_out)) {
    flog.info("Carregando %s", file_out)
    fit <- readRDS(file_out)
  } else {
    flog.info("Salvando em %s", file_out)
  }

  if(!"lasso" %in% names(fit)) {
    flog.info("cv.biglasso")
    fit$coefnames <- attr(x, "fbm_names")
    xtrain <- big_copy(x, ind.row = which(train),
                       backingfile = paste0(bigstatsr::sub_bk(x$bk),"-train"))
    # ftry({
      fit$lasso <- biglasso::cv.biglasso(
        X = xtrain$bm(), y = y[train], ncores = ncores, seed = seed,
        penalty = "lasso", family = "binomial", nfolds = 10)
      saveRDS(fit, file = file_out)
      unlink(xtrain$bk);rm(xtrain);gc()
    # })
  }
  flog.info("roc")
  if(!"test" %in% names(fit)) {
    xtest <- big_copy(x, ind.row = which(!train),
                      backingfile = paste0(bigstatsr::sub_bk(x$bk),"-test"))
    if(is.null(fit$lasso)) return(fit)
    # ftry({
      fit$test <- as.numeric(predict(
        fit$lasso, X = xtest$bm(), type = "response",
        lambda = fit$lasso$lambda.min))
      saveRDS(fit, file = file_out)
    # })
    unlink(xtest$bk);rm(xtest);gc()
  }
  if(!"roc" %in% names(fit)) {
    # ftry({
      fit$roc <- roc(y[!train], fit$test)
      saveRDS(fit, file = file_out)
    # })
  }
  
  flog.info("END")
  return(fit)
}

fit_xgboost <- function(data, train, file_out, seed = NULL, ncores = 1) {
  flog.info("START: Ajuste xgboost - seed %s", seed)
  
  fit <- list()
  if(file.exists(file_out)) {
    flog.info("Carregando %s", file_out)
    fit <- readRDS(file_out)
  } else {
    flog.info("Salvando em %s", file_out)
  }
  
  params <- list(
    nthread = ncores,
    lambda = 0.01,
    objective = "binary:logistic",
    metrics = "error",
    seed = seed
  )
  
  dtrain <- xgboost::slice(data, idxset = which(train))
  colnames(dtrain) <- colnames(data)
  if(!"xgbcv" %in% names(fit)) {
    flog.info("xgb.cv")
    # ftry({
      fit$xgbcv <- xgboost::xgb.cv(params = params, data = dtrain, nround = 10000,
                                   nfold = 2, verbose = FALSE)
      saveRDS(fit, file = file_out)
    # })
  }
  if(!"xgb" %in% names(fit)) {
    flog.info("xgb.train")
    nround <- which.min(fit$xgbcv$evaluation_log$test_error_mean)
    # ftry({
      fit$xgb <- xgboost::xgb.train(params = params, data = dtrain,
                                    nround = nround, verbose = FALSE)
      saveRDS(fit, file = file_out)
    # })
  }
  rm(dtrain); gc()
  
  flog.info("roc")
  dtest <- xgboost::slice(data, idxset = which(!train))
  colnames(dtest) <- colnames(dtest)
  if(!"test" %in% names(fit)) {
    fit$test <- as.numeric(predict(fit$xgb, dtest, type = "response"))
    saveRDS(fit, file = file_out)
  }
  if(!"roc" %in% names(fit)) {
    fit$roc <- roc(xgboost::getinfo(dtest, 'label'), fit$test)
    saveRDS(fit, file = file_out)
  }
  rm(dtest); gc()
  
  flog.info("END")
  return(fit)
}
