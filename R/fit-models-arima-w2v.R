## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
# cases_w2v <- file.path(".","output","track_w2v.txt") %>%
#   read.table(., header = T, stringsAsFactors = F) %>%
#   dplyr::select(., everything(), file = file_out) %>%
#   dplyr::rename_all(., .funs = function(x) paste0("w2v_", x))
# cases_tw2v <- file.path(".","output","track_text_w2v.txt") %>%
#   read.table(., header = T, stringsAsFactors = F) %>%
#   dplyr::select(., everything(), tw2v_file = file_out)
# cases <- merge(cases_tw2v, cases_w2v)

# cases_rm <- file.path(".","output","track_arima_w2v.txt") %>%
#   read.table(., header = T, stringsAsFactors = F)
# cases <- merge(cases, cases_rm, all = T) %>%
#   dplyr::filter(., is.na(file_out))
# rm(cases_w2v,cases_tw2v,cases_rm)

cases <- file.path(".","output","track_text_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(tw2v_file = file_out) %>% unlist(.)
con <- file.path(".","output","tw2v_var.sqlite") %>%
  DBI::dbConnect(RSQLite::SQLite(), dbname = .)
cases_rm <- dbGetQuery(con, "SELECT DISTINCT tw2v_file FROM tw2v_var") %>% unlist(.)
cases <- setdiff(cases, cases_rm) 
rm(cases_rm)

## ----

# dbSendQuery(conn = con, "CREATE TABLE tw2v_var (tw2v_file TEXT, dim INTEGER, var REAL)")
# dbRemoveTable(conn = con, name = "tw2v_var")

r <- cases[-c(1:3)] %>%
  lapply(., function(case) {
    try({
      tw2v <- read.vectors(filename = as.character(case))@.Data
      x <- lapply(seq(ncol(tw2v)), function(idx) var(tw2v[,idx])) %>%
        unlist(.) %>% data.frame(var = .) %>%
        mutate(tw2v_file = case, dim = row_number())
      dbWriteTable(conn = con, name = "tw2v_var", x, append = TRUE)
    })
    gc(verbose = F, full = F)
  })

dbDisconnect(con)

## global ----
cases_sample <- file.path(".","output","track_sample.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::rename_all(., .funs = function(x) paste0("sample_", x))
cases_w2v <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x))
cases <- tidyr::crossing(cases_w2v, cases_sample) %>%
  tidyr::crossing(seed=1:1, stat=c("var","mean+var"),
                  model=c("lasso","xgb")) %>% # linux: lasso-grid = lasso
  dplyr::mutate(ncores = ifelse(model == "xgb", no_cores, NA)) %>%
  dplyr::filter(., w2v_dim == 100)
cases_rm <- file.path(".","output","track_model_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_out))
rm(cases_sample,cases_w2v,cases_rm)

# create directories where models will be saved
path_out <- file.path(".","output","model")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

## load data ----
bow <- file.path(".","output","bow","bow.rds") %>%
  readRDS(.) %>% as(.,"sparseMatrix")
# con <- file.path(".","output","tw2v_var.sqlite") %>%
#   DBI::dbConnect(RSQLite::SQLite(), dbname = .)
# 
# tvar <- dbGetQuery(con, "SELECT * FROM tw2v_var") %>%
#   spread(dim,var)
# idx <- tvar$tw2v_file %>%
#   stringr::str_extract(.,"_text.*_") %>%
#   stringr::str_remove_all(., "_|(text)") %>%
#   as.numeric(.) #
# tvar <- tvar %>% dplyr::select(-tw2v_file) %>% as.matrix(.)

# tvar %>% mutate(text = idx,dim = 100) %>%
#   dplyr::select(-tw2v_file) %>%
#   write.table(x = ., file = file.path(".","output","tw2v_var.txt"))
tvar <- read.table(file.path(".","output","tw2v_var.txt"))
idx <- tvar$text
tvar <- tvar %>% dplyr::select(-dim,-text) %>% as.matrix(.)
tvar[is.na(tvar)] <- 0

## fit models: lasso + xgboost ----
t <- cases %>% split(., .$w2v_file) %>%
  lapply(., function(cases) {
    w2v <- cases$w2v_file[1] %>% read.vectors(.)
    w2v <- w2v[colnames(bow),]
    
    tmean <- (bow %*% w2v) %>% as.matrix() # sum/wsum tem resultados parecidos
    tmean <- tmean[idx,] #

    split(cases, seq(nrow(cases))) %>%
      lapply(., function(case) {
        description <- paste0("w2v",case$w2v_dim,"-",case$stat)
        x <- if(case$stat == "var") { tvar } else { cbind(tmean,tvar) }
        y <- load_labels(indic_geq = case$sample_igeq)[idx]
        train <- gen_train(size = length(y), seed = case$sample_seed,
                           prob = case$sample_prob)

        file <- paste0(case$model,"_",description) %>%
          namefile(path = path_out, str = ., extension = ".rds")
        case <- case %>% dplyr::mutate(file_out = file) %>%
          dplyr::select(sample_id,ncores,seed,stat,model,w2v_file,file_out)
        file.path(".","output","track_model_w2v.txt") %>%
          write.table(x = case, file = ., append = file.exists(.),
                      col.names = !file.exists(.), row.names = F)
        
        if(str_detect(case$model, pattern = "lasso")) {
          fit <- fit_lasso(
            x = x, y = y, train = train, file_out = file,
            description = description, seed = case$seed,
            with_grid = (case$model != "lasso"))
        } else {
          fit <- fit_xgboost(
            x = x, y = y, train = train, file_out = file,
            description = description, seed = case$seed, no_cores = case$ncores)
        }
      })
  })

## plot ----
ggpath <- file.path(".","plots","model-performance2")

cases_sample <- file.path(".","output","track_sample.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::rename_all(., .funs = function(x) paste0("sample_", x))
cases_w2v <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x))
cases_bow <- file.path(".","output","track_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("bow_", x)) %>%
  # dplyr::filter(., is.na(bow_k)) %>% # nao ignorar clusters
  dplyr::mutate(., npl = "bow")

cases_mbow <- file.path(".","output","track_model_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  list(.,cases_sample,cases_bow) %>%
  Reduce(function(x, y) merge(x, y), .)
cases_mw2v <- file.path(".","output","track_model_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  list(.,cases_sample,cases_bow,cases_w2v) %>%
  Reduce(function(x, y) merge(x, y), .) %>%
  dplyr::mutate(npl = paste0("w",w2v_dim,"-",stat))
cases <- merge(cases_mbow,cases_mw2v,all=T)
rm(cases_sample,cases_bow,cases_w2v,cases_mbow,cases_mw2v)

gg <- cases %>%
  dplyr::filter(., is.na(bow_k) & sample_id < 3) %>%
  dplyr::filter(., stat %in% c("sum","var","mean+var") & w2v_dim %in% c(NA,100)) %>%
  split(., list(.$sample_id,.$seed)) %>%
  lapply(., function(cases) {
    roc <- cases %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(x) {
        if(!file.exists(x$file_out)) return(NA)
        data <- readRDS(x$file_out)
        
        if("roc" %in% names(data)) {
          data <- data[["roc"]] %>% "["(c("sensitivities","specificities")) %>%
            do.call(cbind, .) %>% as.data.frame() %>% cbind(x,.)
        } else { # arq antigo :(
          print(x$file_out)
          data <- data[[x$model]][["roc"]] %>% "["(c("sensitivities","specificities")) %>%
            do.call(cbind, .) %>% as.data.frame() %>% cbind(x,.)
        }
        return(data)
      }) %>% do.call(rbind, .)
    
    title <- with(cases[1,], paste("Sample: train-prob",sample_prob,"| igeq",sample_igeq))
    ggplot(data = roc, aes(specificities,sensitivities,
                           group=file_out,col=npl,linetype=model)) +
      geom_line() + labs(title = title, x = "Especificidade", y = "Sensibilidade")
    paste0("roc_sample-id-",cases$sample_id[1],"_var") %>%
      ggsave2(filename = ., path = ggpath)
  })

