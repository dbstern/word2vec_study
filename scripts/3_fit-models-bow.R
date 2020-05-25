## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## list cases stats ----
cases_sample <- get_relpath("sample",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
cases_bow <- get_relpath("bow",NULL) %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(., rmstopw == .GlobalEnv$rmstopw)
cases <- tidyr::crossing(cases_bow, cases_sample) %>%
  tidyr::crossing(
    model_seed = 1:1,
    modbow = if(db=="proc") c(F,T) else NULL,
    label = colnames(load_label()),
    model = c("lasso","xgb"),
    model_ncores = no_cores
  ) %>%
  dplyr::filter(., (model != "xgb") | (model == "xgb" & k%%200 %in% c(NA,0)))
cases_rm <- get_relpath("mbow",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_model))
rm(cases_sample,cases_bow,cases_rm)

## convert to stats files to libsvm ----
if(db == "amzn") {
  cases_bow <- get_relpath("bow",NULL) %>%
    read.table(., header = T, stringsAsFactors = F) %>%
    dplyr::filter(., rmstopw == .GlobalEnv$rmstopw)
  x <- lapply(cases_bow$file_bow, function(file) {
    file <- get_relpath("bow",file)
    # if(file.info(file)$size < as.numeric(benchmarkme::get_ram()/10)) return(NULL)
    tmpfile <- stringr::str_replace(file, pattern="rds", replacement="txt")
    if(!file.exists(tmpfile)) {
      bow <- readRDS(file) %>% as.matrix(.)
      write.table(x = bow, file = tmpfile, sep = ' ', row.names = F)
    }
    newfile <- stringr::str_replace(file, pattern="rds", replacement="dat")
    if(file.exists(newfile)) return(NULL)
    print(newfile)
    str <- sprintf("python ./scripts/ssv2libsvm.py %s %s %i %i", tmpfile, newfile, -1, 1)
    pipe(str,"w")
    # str <- sprintf("rm %s", tmpfile)
    # pipe(str,"w")
  })
  rm(cases_bow,x)
}

## fit models: lasso + xgboost ----
cases <- cases %>%
  # dplyr::filter(., label %in% c("acq","crude","earn","grain","money.fx")) %>%
  dplyr::filter(., is.na(file_clw2v)) %>%
  # dplyr::filter(., model == "lasso") %>%
  dplyr::filter(., sample_id == 1)

r <- cases %>%
  split(., seq(nrow(.))) %>%
  lapply(., function(case) {
    file <- get_relpath("mbow",T)
    case_cols <- c("sample_id","model_ncores","model_seed",
                   "modbow","label","model","file_bow","file_model")
    case_save <- dplyr::mutate(case, file_model = basename(file)) %>%
      dplyr::select(., one_of(case_cols))
    
    # bow <- get_relpath("bow",case$file_bow) %>%
    #   readRDS(file) %>% as.matrix(.)
    # if(!is.null(case$modbow)) if(case$modbow)
    #   bow <- bow[,-which(str_detect(colnames(bow), pattern = "procedente"))]
    y <- load_label(indic_geq = case$sample_igeq, label = case$label) %>% as.numeric(.)
    train <- gen_train(size = length(y), seed = case$sample_seed, prob = case$sample_prob)
    
    flog.info(paste(names(case_save),case_save,collapse = "\t",sep = ":"))
    tryCatch({
      x <- load_bow(file = get_relpath("bow",case$file_bow), 
                      model = case$model, modbow = case$modbow)
    }, warning=function(w) {
      flog.warn(w)
      return(NULL)
    }, error=function(e) {
      fbmfiles <- dir(path = get_relpath("bow",""), pattern = ".\\.(rds)|(bk)", full.names = T)
      unlink(fbmfiles)
      flog.error(e)
      return(NULL)
    })
    
    get_relpath("mbow",NULL) %>%
      write.table(x = case_save, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    error <- tryCatch({
      fbmfiles <- c()
      fit <- if(case$model == "lasso") {
        if(is.matrix(x)) #??
          x <- as_FBM(x, backingfile = get_relpath("bow",case$file_bow))
        fbmfiles <- c(x$bk,x$rds)
        fit <- fit_lasso(
          x = x, y = y, train = train, file_out = file,
          seed = case$model_seed, ncores = case$model_ncores)
        unlink(fbmfiles)
        fit
      } else {
        if(class(x) != "xgb.DMatrix") x <- xgb.DMatrix(x)
        xgboost::setinfo(object = x, 'label', as.numeric(y))
        fit_xgboost(
          data = x, train = train, file_out = file,
          seed = case$model_seed, ncores = case$model_ncores)
      }
      NULL
    }, warning=function(w) {
      flog.warn(w)
      return(NULL)
    }, error=function(e) {
      unlink(fbmfiles)
      flog.info("END")
      get_relpath("mbow","cases_error.txt") %>%
        write.table(case, file = ., append = file.exists(.),
                    row.names = F, col.names = !file.exists(.))
      NULL
    }, finally = {
      unlink(fbmfiles)
    })
    
    # error <- tryCatch({
    #   file_bk <- c()
    #   fit <- if(case$model == "lasso") {
    #     x <- as_FBM(bow, backingfile = get_relpath("bow",case$file_bow))
    #     file_bk <- x$bk
    #     fit <- fit_lasso(
    #       x = x, y = y, train = train, file_out = file,
    #       seed = case$model_seed, ncores = case$model_ncores)
    #   } else {
    #     x <- xgb.DMatrix(bow, label = y)
    #     fit_xgboost(
    #       data = x, train = train, file_out = file,
    #       seed = case$model_seed, ncores = case$model_ncores)
    #   }
    #   get_relpath("mbow",NULL) %>%
    #     write.table(x = case_save, file = ., append = file.exists(.),
    #                 col.names = !file.exists(.), row.names = F)
    #   NULL
    # }, warning=function(w) {
    #   flog.warn(w)
    #   return(NULL)
    # }, error=function(e) {
    #   unlink(file_bk)
    #   flog.info("END")
    #   get_relpath("mbow","cases_error.txt") %>%
    #     write.table(case, file = ., append = file.exists(.),
    #                 row.names = F, col.names = !file.exists(.))
    #   NULL
    # }, finally = {
    #   unlink(file_bk)
    # })
    
    rm(x); gc()
    error
  })
