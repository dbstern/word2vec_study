## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
cases <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x)) %>%
  tidyr::crossing(seed = 1:1, stat = c("sum","wsum"), model = c("lasso","lasso-grid","xgb")) %>%
  # tidyr::crossing(seed = 1:1, stat = c("sum","wsum"), model = c("lasso","xgb")) %>%
  dplyr::mutate(ncores = ifelse(model == "xgb", no_cores,  NA))
cases_rm <- file.path(".","output","track_model-w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::mutate(ncores = ifelse(model == "xgb", no_cores,  NA))
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_out))
rm(cases_rm)


# create directories where models will be saved
path_out <- file.path(".","output","model")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

## load data ----
y <- file.path(".","input","labels_clean.txt") %>%
  read.table(file = ., header = FALSE, sep = "\t", stringsAsFactors = FALSE) %>% "[["(1)
train <- gen_train(size = length(y))
bow <- file.path(".","output","bow","bow.rds") %>%
  readRDS(.) %>% as(.,"sparseMatrix")

## fit models: lasso + xgboost ----
t <- cases %>% split(., .$w2v_file) %>%
 lapply(., function(cases) {
   w2v <- cases$w2v_file[1] %>% read.vectors(.)
   w2v <- w2v[colnames(bow),]
   
   split(cases, seq(nrow(cases))) %>%
     lapply(., function(case) {
       description <- paste0("w2v",case$w2v_dim,"-",case$stat)
       
       xbow <- bow / if(case$stat == "sum") 1 else rowSums(bow)
       w2v <- (xbow %*% w2v) %>% as.matrix()
       
       file <- paste0(case$model,"_",description) %>%
         namefile(path = path_out, str = ., extension = ".rds")
       case <- case %>% dplyr::mutate(file_out = file) %>%
         dplyr::select(ncores,seed,stat,model,w2v_file,file_out)
       file.path(".","output","track_model-w2v.txt") %>%
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

