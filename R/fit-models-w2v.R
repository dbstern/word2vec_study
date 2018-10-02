## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
cases <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x)) %>%
  tidyr::crossing(ncores = no_cores, seed = 1:1, stat = c("sum","wsum"))

# create directories where models will be saved
path_out <- file.path(".","output","model")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

## load data ----
y <- read.table(file = file.path("input","labels_clean.txt"),
                header = FALSE, sep = "\t", stringsAsFactors = FALSE)[[1]]
train <- gen_train(size = length(y))
bow <- readRDS(file.path("output","bow","bow.rds")) %>%
  as(.,"sparseMatrix")

## fit models: lasso + xgboost ----
t <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    xbow <- bow / if(case$stat == "sum") 1 else rowSums(bow)
    
    w2v <- case$w2v_file %>% read.vectors(.)
    w2v <- w2v[colnames(bow),]
    w2v <- (xbow %*% w2v) %>% as.matrix()
    
    description <- paste0("w2v",case$w2v_dim,"-",case$stat)
    models <- c("lasso","lasso-grid","xgb")
    files <- paste0(models,"_",description) %>%
      namefile(path = path_out, str = ., extension = ".rds")
    cases <- case %>%
      dplyr::select(ncores,seed,stat,w2v_file) %>%
      tidyr::crossing(model = models) %>%
      dplyr::mutate(file_out = files)
    file.path(".","output","track_model-w2v.txt") %>%
      write.table(x = cases, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    fit <- fit_lasso(
      x = w2v, y = y, train = train, file_out = files[1],
      description = description, seed = case$seed)
    fit <- fit_lasso(
      x = w2v, y = y, train = train, file_out = files[2],
      description = description, seed = case$seed, with_grid = T)
    fit <- fit_xgboost(
      x = w2v, y = y, train = train, file_out = files[3],
      description = description, seed = case$seed, no_cores = case$ncores)
    return(NULL)
  })

