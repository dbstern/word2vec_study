## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
cases_sample <- file.path(".","output","track_sample.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::rename_all(., .funs = function(x) paste0("sample_", x))
cases_bow <- file.path(".","output","track_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("bow_", x))
cases <- tidyr::crossing(cases_bow, cases_sample) %>%
  tidyr::crossing(seed=1:1, model=c("lasso","xgb")) %>% # linux: lasso-grid = lasso
  dplyr::mutate(ncores = ifelse(model == "xgb", no_cores,  NA))
cases_rm <- file.path(".","output","track_model-bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_out)) %>%
  dplyr::filter(., (model != "xgb")| (model == "xgb" & bow_k%%200 %in% c(NA,0)))
rm(cases_sample,cases_bow,cases_rm)

# create directories where models will be saved
path_out <- file.path(".","output","model")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

## fit models: lasso + xgboost ----
t <- cases %>% split(., .$bow_file) %>%
  lapply(., function(cases) {
    bow <- readRDS(cases$bow_file[1]) %>% as(.,"sparseMatrix")
    description <- paste0("bow", if(!is.na(cases$bow_cluster[1])) "-cl")
    
    split(cases, seq(nrow(cases))) %>%
      lapply(., function(case) {
        y <- load_labels(indic_geq = case$sample_igeq)
        train <- gen_train(size = length(y), seed = case$sample_seed,
                           prob = case$sample_prob)
        
        file <- paste0(case$model,"_",description) %>%
          namefile(path = path_out, str = ., extension = ".rds")
        case <- case %>%
          dplyr::mutate(file_out = file) %>%
          dplyr::select(sample_id,ncores,seed,model,bow_file,file_out)
        file.path(".","output","track_model-bow.txt") %>%
          write.table(x = case, file = ., append = file.exists(.),
                      col.names = !file.exists(.), row.names = F)
        
        if(str_detect(case$model, pattern = "lasso")) {
          fit <- fit_lasso(
            x = bow, y = y, train = train, file_out = file,
            description = description, seed = case$seed,
            with_grid = (case$model != "lasso"))
        } else {
          fit <- fit_xgboost(
            x = bow, y = y, train = train, file_out = file,
            description = description, seed = case$seed, no_cores = case$ncores)
        }
      })
  })

## global ggplot - compare clusters ----
ggpath <- file.path(".","plots","model-performance")

bow <- readRDS(file.path("output","bow","bow.rds")) %>% as(.,"sparseMatrix")
nbow <- ncol(bow)
rm(bow)

cases_sample <- file.path(".","output","track_sample.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::rename_all(., .funs = function(x) paste0("sample_", x))
cases_bow <- file.path(".","output","track_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("bow_", x))
cases_model <- file.path(".","output","track_model-bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases_sample,cases_bow) %>%
  merge(.,cases_model) %>%
  mutate(bow_k = ifelse(is.na(bow_k), nbow, bow_k))
rm(cases_sample,cases_model, cases_bow)

## compare auc ----
cases$auc <- cases %>% 
  split(., seq(nrow(.))) %>%
  lapply(., function(x) {
    if(!file.exists(x$file_out)) return(NA)
    readRDS(x$file_out)[["roc"]][["auc"]][[1]]
  }) %>%
  unlist()

gg <- cases %>% split(., .$sample_igeq) %>%
  lapply(., function(cases) {
    title <- with(
      cases[1,], paste("Sample: igeq",sample_igeq,"\nCluster method:",bow_method))
    
    gg <- cases %>% ggplot(., aes(bow_k, auc))
    # gg + geom_point() + facet_grid(model ~ sample_prob)
    gg + geom_point(aes(col = model), position = position_jitter(w = 10, h = 0)) +
      facet_grid(sample_prob ~ .) + labs(title = title, x = "# clusters", y = "AUC")
    paste0("auc_model-bow_npl-cluster_sample-igeq-",cases$sample_igeq[1]) %>%
      ggsave2(filename = ., path = ggpath)
  })

## compare roc ----
gg <- cases %>%
  dplyr::filter(., bow_k%%400==0 | bow_k==nbow) %>%
  split(., c(.$sample_id, .$bow_cluster, .$bow_method, .$seed)) %>%
  lapply(., function(cases) {
    roc <- cases %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(x) {
        if(!file.exists(x$file_out)) return(NA)
        readRDS(x$file_out)[["roc"]] %>%
          "["(c("sensitivities","specificities")) %>%
          do.call(cbind, .) %>% as.data.frame() %>% cbind(x,.)
      }) %>%
      do.call(rbind, .)
    
    title <- with(
      cases[1,], paste("Sample: train-prob",sample_prob,"| igeq",sample_igeq,"\nCluster method:",bow_method))
    ggplot(data = roc, aes(specificities,sensitivities,
                           group=file_out,col=bow_k,linetype=model)) +
      geom_line() + labs(title = title, x = "Especificidade", y = "Sensibilidade")
    paste0("roc_model-bow_npl-cluster_sample-id-",cases$sample_id[1]) %>%
      ggsave2(filename = ., path = ggpath)
  })


