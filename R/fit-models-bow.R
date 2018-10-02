## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
cases <- file.path(".","output","track_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("bow_", x)) %>%
  tidyr::crossing(seed = 1:1, model = c("lasso","lasso-grid","xgb")) %>%
  dplyr::mutate(ncores = ifelse(model == "xgb", no_cores,  NA))
cases_rm <- file.path(".","output","track_model-bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., -model)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_out)) %>%
  dplyr::filter(., (model != "xgb")| (model == "xgb" & bow_k%%200 == 0))
rm(cases_rm)

# create directories where models will be saved
path_out <- file.path(".","output","model")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)
ggpath <- file.path(".","plots","model-performance")

## load data ----
y <- read.table(file = file.path("input","labels_clean.txt"),
                header = FALSE, sep = "\t", stringsAsFactors = FALSE)[[1]]
train <- gen_train(size = length(y))

## fit models: lasso + xgboost ----
t <- cases %>% split(., .$bow_file) %>%
  lapply(., function(cases) {
    bow <- readRDS(cases$bow_file[1]) %>% as(.,"sparseMatrix")
    description <- paste0("bow", if(!is.na(cases$bow_cluster[1])) "-cl")
    
    split(cases, seq(nrow(cases))) %>%
      lapply(., function(case) {
        file <- paste0(case$model,"_",description) %>%
          namefile(path = path_out, str = ., extension = ".rds")
        case <- case %>%
          dplyr::mutate(file_out = file) %>%
          dplyr::select(ncores,seed,model,bow_file,file_out)
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

## compare roc between clusters ----
cases_bow <- file.path(".","output","track_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("bow_", x))
cases_model <- file.path(".","output","track_model-bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases_bow, cases_model) %>%
  mutate(bow_k = ifelse(is.na(bow_k), 0, bow_k))
rm(cases_model, cases_bow)

cases$auc <- cases %>% 
  split(., seq(nrow(.))) %>%
  lapply(., function(x) {
    if(!file.exists(x$file_out)) return(NA)
    readRDS(x$file_out)[["roc"]][["auc"]][[1]]
  }) %>%
  unlist()

gg <- cases %>% ggplot(., aes(bow_k, auc))
gg + geom_point() + facet_grid(model ~ .)
gg + geom_point(aes(col = model), position = position_jitter(w = 10, h = 0))
ggsave2(filename = "auc_model-bow_npl-cluster", path = ggpath)
