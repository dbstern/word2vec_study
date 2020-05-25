## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## list cases models ----
cases_sample <- get_relpath("sample",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
cases_d2v <- get_relpath("d2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(., rmstopw == .GlobalEnv$rmstopw)
cases <- tidyr::crossing(cases_d2v, cases_sample) %>%
  tidyr::crossing(
    model_seed=1:1,
    stat=c("dm","dm+dbow"),
    label = colnames(load_label()),
    model=c("lasso","xgb"),
    model_ncores = no_cores
  )

if(file.exists(get_relpath("md2v",NULL))) {
  cases_rm <- get_relpath("md2v",NULL) %>%
    read.table(., header = T, stringsAsFactors = F)
  cases <- merge(cases, cases_rm, all = T) %>%
    dplyr::filter(., is.na(file_model))
  rm(cases_rm)
} else {
  cases$file_model <- NA
}
if(file.exists(get_relpath("md2v","cases_error.txt"))) {
  cases_error <- get_relpath("md2v","cases_error.txt") %>%
    read.table(., header = T, stringsAsFactors = F) %>%
    dplyr::mutate(., file_model = "error") %>% unique(.)
  cases <- cases %>% dplyr::select(., -file_model) %>%
    merge(., cases_error, all = T) %>%
    dplyr::filter(., is.na(file_model))
  rm(cases_error)
}
rm(cases_sample,cases_d2v)

## fit models ----
cases <- cases %>%
  dplyr::filter(., sample_id == 1) %>%
#   dplyr::filter(., dim == 100) %>%
#   dplyr::filter(., model == "lasso") %>%
  dplyr::filter(., label %in% c("acq","crude","earn","grain","money.fx")) #%>%
#   dplyr::filter(., sample_prob == .5)

cases_error <- cases %>%
  split(., seq(nrow(.)), drop = T) %>%
  lapply(., function(case) {
    file <- ifelse(is.na(case$file_model), get_relpath("md2v",T), case$file_model)
    case_cols <- c("sample_id","model_ncores","model_seed","stat","label","model","file_d2v","file_model")
    case <- dplyr::mutate(case, file_model = basename(file))
    case_save <- dplyr::select(case, one_of(case_cols))
    
    y <- load_label(indic_geq = case$sample_igeq, label = case$label) %>% as.numeric(.)
    train <- gen_train(size = length(y), seed = case$sample_seed, prob = case$sample_prob)

    flog.info(paste(names(case),case,collapse = "\t",sep = ":"))
    tryCatch({
      x <- load_d2v(file = get_relpath("d2v",case$file_d2v), dim = case$dim,
                    stat = case$stat, model = case$model)
    }, warning=function(w) {
      flog.warn(w)
      return(NULL)
    }, error=function(e) {
      fbmfiles <- dir(path = get_relpath("d2v",""), pattern = ".\\.(rds)|(bk)", full.names = T)
      unlink(fbmfiles)
      flog.error(e)
      return(NULL)
    })

    get_relpath("md2v",NULL) %>%
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
      get_relpath("md2v","cases_error.txt") %>%
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
