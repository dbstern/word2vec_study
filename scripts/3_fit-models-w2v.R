## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## load data ----
bow <- get_relpath("bow",T) %>%
  readRDS(.) %>% as(.,"sparseMatrix")
txt <- load_txt()

## list cases stats ----
cases_w2v <- get_relpath("w2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(., rmstopw == .GlobalEnv$rmstopw)
cases <- cases_w2v %>%
  tidyr::crossing(
    modbow = if(db=="proc") c(F,T) else NULL,
    # modbow=c(F,if(db=="proc") T),
    return=c(F,T),
    dred_w2v=c(F,"pca","tsne")
  ) %>%
  dplyr::filter(., (dim > 2) | (dim==2 & dred_w2v=="FALSE"))

if(file.exists(get_relpath("stw2v",NULL))) {
  cases_rm <- get_relpath("stw2v",NULL) %>%
    read.table(., header = T, stringsAsFactors = F)
  cases <- merge(cases, cases_rm, all = T) %>%
    dplyr::filter(., is.na(file_stw2v))
  rm(cases_rm)
}
rm(cases_w2v)

## calculate stats ----
cases <- cases %>%
  dplyr::filter(., !return) %>%
  dplyr::filter(., dred_w2v=="FALSE") %>%
  dplyr::filter(., !return) #%>%
  # dplyr::filter(., dim == 100)

x <- cases %>%
  split(., seq(nrow(.)), drop = T) %>%
  lapply(., function(case) {
    w2v <- get_relpath("w2v",case$file_w2v) %>% read.vectors(.)
    w2v <- get_w2v_dred(w2v, dred = case$dred_w2v)
    
    file <- get_relpath("stw2v",T)
    file_bk <- str_replace(file, ".txt", "_")
    flog.info("START: stw2v - dim %s - return %s - dred_w2v %s",
              case$dim, case$return, case$dred_w2v)
    
    # case_cols <- c(if(db=="proc")"modbow","return","dred_w2v","file_w2v","file_stw2v")
    case_cols <- c("modbow","return","dred_w2v","file_w2v","file_stw2v")
    case_save <- dplyr::mutate(case, file_stw2v = basename(file)) %>%
      dplyr::select(., one_of(case_cols))
    get_relpath("stw2v",NULL) %>%
      write.table(x = case_save, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    # if(case$modbow)
    if(!is.null(case$modbow)) if(case$modbow)
      w2v <- w2v[-which(str_detect(rownames(w2v), pattern = "procedente")),]
    
    stw2v <- FBM(nrow = length(txt), ncol = 105*ncol(w2v), backingfile = file_bk)
    cl <- parallel::makeCluster(no_cores, outfile = "")
    doParallel::registerDoParallel(cl)
    x <- foreach(idx = seq(length(txt)), .export = c("w2v","txt","get_returns","logpath"),
                 .packages = c("tidyverse","fBasics","futile.logger")) %dopar%
      {
        flog.appender(appender.file(logpath))
        tryCatch({
          words <- txt[idx] %>% str_split(., "[[:space:]]")
          tw2v <- w2v[match(words[[1]], row.names(w2v), nomatch = 0),,drop=F]
          if(case$return) tw2v <- get_returns(tw2v)
          
          x <- data.frame(text = idx, dim = sprintf("%03d", seq(ncol(w2v)))) %>%
            dplyr::mutate(
              mean = colMeans(x = tw2v),
              sd = colSds(tw2v),
              skew = colSkewness(x = tw2v),
              kurt = colKurtosis(x = tw2v)
            )
          quant <- data.frame(matrixStats::colQuantiles(x = tw2v, probs = seq(0,100,1)/100))
          names(quant) <- sprintf("quant%03d", seq(0,100))
          x <- cbind(x, quant)
          if(sum(is.na(x)) > 0) x[is.na(x)] <- 0
          
          x <- x %>%
            tidyr::gather(stat,value,-text,-dim) %>%
            tidyr::unite(temp, stat, dim) %>%
            tidyr::spread(temp, value) %>%
            dplyr::arrange(., text) %>%
            dplyr::select(.,-text) %>%
            as.matrix(.)
          
          stw2v[idx,] <- x
          if(idx == 1) colnames(x) else NULL
        }, error=function(e) {
          flog.error(e)
        })
      }
    parallel::stopCluster(cl)
    stw2v <- stw2v$bm()
    colnames(stw2v) <- x[[1]]
    write.big.matrix(x = stw2v, filename = file,  col.names = T,
                     row.names = F, sep = " ")
    unlink(paste0(file_bk,".bk"))
    rm(stw2v,x)
    
    gc(verbose = T, full = T)
    flog.info("END")
    return(NULL)
  })
rm(x,cases)

## convert to stats files to libsvm ----
cases_stw2v <- get_relpath("stw2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
x <- lapply(cases_stw2v$file_stw2v, function(file) {
  file <- get_relpath("stw2v",file)
  if(file.info(file)$size < as.numeric(benchmarkme::get_ram()/10)) return(NULL)
  newfile <- stringr::str_replace(file, pattern="txt", replacement="dat")
  if(file.exists(newfile)) return(NULL)
  print(newfile)
  str <- sprintf("python ./scripts/ssv2libsvm.py %s %s %i %i", file, newfile, -1, 1)
  pipe(str,"w")
})
rm(cases_stw2v,x)

## list cases models ----
cases_sample <- get_relpath("sample",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
cases_w2v <- get_relpath("w2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(., rmstopw == .GlobalEnv$rmstopw)
cases_stw2v <- get_relpath("stw2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)

cases <- dplyr::inner_join(cases_w2v,cases_stw2v) %>%
  tidyr::crossing(., cases_sample) %>%
  tidyr::crossing(
    model_seed=1:1,
    stat=c("mean","mean+sd","mean+sd+skew","mean+sd+skew+kurt",
           "mean+sd+skew+kurt+quant",
           "mean+sd+skew+kurt+tailquant",
           # "mean+sd+skew+kurt+quart+tailquant",
           "unifquant",
           "mixquant",
           # "mean+rawmean",
           paste0("pca_dimx",c(#10,
             20))
    ),
    label = colnames(load_label()),
    model=c("lasso","xgb"),
    model_ncores = no_cores
  ) %>%
  # dplyr::mutate(., stat = ifelse(!stringr::str_detect(stat,"pca"),stat,
  #                                paste0("pca_",dim*as.numeric(stringr::str_remove(stat,"pca_"))))) %>%
  dplyr::filter(., !(!return & stat == "mean+rawmean") &
                  !(stringr::str_detect(stat,"pca") & (dim==2 | dred_w2v!="FALSE")))

if(file.exists(get_relpath("mw2v",NULL))) {
  cases_rm <- get_relpath("mw2v",NULL) %>%
    read.table(., header = T, stringsAsFactors = F)
  cases <- merge(cases, cases_rm, all = T) %>%
    dplyr::filter(., is.na(file_model))
  rm(cases_rm)
} else {
  cases$file_model <- NA
}
if(file.exists(get_relpath("mw2v","cases_error.txt"))) {
  cases_error <- get_relpath("mw2v","cases_error.txt") %>%
    read.table(., header = T, stringsAsFactors = F) %>%
    dplyr::mutate(., file_model = "error") %>% unique(.)
  cases <- cases %>% dplyr::select(., -file_model) %>%
    merge(., cases_error, all = T) %>%
    dplyr::filter(., is.na(file_model))
  rm(cases_error)
}
rm(cases_sample,cases_w2v,cases_stw2v)

## fit models ----
cases <- cases %>%
  dplyr::filter(., !return) %>%
  dplyr::filter(., sample_id == 1) %>%
  # dplyr::filter(., dim == 100) %>%
  # dplyr::filter(., model == "lasso") %>%
  dplyr::filter(., dred_w2v == "FALSE") #%>%
  # dplyr::filter(., label %in% c("acq","crude","earn","grain","money.fx")) #%>%
  # dplyr::filter(., sample_prob == .5) %>%
  # dplyr::filter(., !modbow) %>%
  # dplyr::filter(., str_detect(stat,"pca")) #%>%
  # dplyr::filter(., str_detect(stat,"[+]quant"))
  
cases_error <- cases %>%
  split(., seq(nrow(.)), drop = T) %>%
  lapply(., function(case) {
    file <- ifelse(is.na(case$file_model), get_relpath("mw2v",T), case$file_model)
    case_cols <- c("sample_id","model_ncores","model_seed","modbow","return",
                   "dred_w2v","stat","label","model","file_w2v","file_model")
    case <- dplyr::mutate(case, file_model = basename(file))
    case_save <- dplyr::select(case, one_of(case_cols))
    
    # y <- load_label(indic_geq = case$sample_igeq) %>% as.numeric(.)
    y <- load_label(indic_geq = case$sample_igeq, label = case$label) %>% as.numeric(.)
    train <- gen_train(size = length(y), seed = case$sample_seed, prob = case$sample_prob)
    
    # w2v <- get_relpath("w2v",case$file_w2v) %>% read.vectors(.)
    # words <- match(colnames(bow), row.names(w2v), nomatch = 0)
    # if(length(which(words==0)) > 0) bow <- bow[,-which(words==0)]
    # bow <- bow %>% as.matrix(.)
    # rm(w2v,words)
    
    flog.info(paste(names(case),case,collapse = "\t",sep = ":"))
    # x <- load_stw2v_old(get_relpath("stw2v",case$file_stw2v), case$stat, case$model)
    tryCatch({
      x <- load_stw2v(file = get_relpath("stw2v",case$file_stw2v), stat = case$stat,
                      model = case$model, train = train, ncores = case$model_ncores)
    }, warning=function(w) {
      flog.warn(w)
      return(NULL)
    }, error=function(e) {
      fbmfiles <- dir(path = get_relpath("stw2v",""), pattern = ".\\.(rds)|(bk)", full.names = T)
      unlink(fbmfiles)
      flog.error(e)
      return(NULL)
    })
    # if(stringr::str_detect(case$stat,"rawmean")) {
    #   x_raw <- case %>%
    #     dplyr::mutate(., return=F, file_stw2v=NULL, stat="mean") %>%
    #     merge(., cases) %>% pull(., file_stw2v)
    #   load_stw2v(., "mean", case$model)
    #   x <- merge(x, x_raw)
    #   rm(x_raw)
    # }
    
    get_relpath("mw2v",NULL) %>%
      write.table(x = case_save, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    error <- tryCatch({
      fbmfiles <- c()
      fit <- if(case$model == "lasso") {
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
      get_relpath("mw2v","cases_error.txt") %>%
        write.table(case, file = ., append = file.exists(.),
                    row.names = F, col.names = !file.exists(.))
      NULL
    }, finally = {
      unlink(fbmfiles)
    })
    
    rm(x); gc()
    error
  }) %>% do.call(rbind,.)


##  ----
