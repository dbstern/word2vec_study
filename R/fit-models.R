## global ----
seed_model_list <- 1:5 # will be set before running each model
seed_w2v_list <- 1:10

# create directories where models will be saved
path_out <- file.path(".","output",c("lasso","xgboost"))
names(path_out) <- c("lasso","xgb")
x <- lapply(path_out, function(path)
  if(!dir.exists(path)) dir.create(path, recursive = T)
); rm(x)

## dependencias ----
source(file.path("R","dependencies.R"))

## load data ----
y <- read.table(file = file.path("input","labels_clean.txt"),
                header = FALSE, sep = "\t", stringsAsFactors = FALSE)[[1]]
train <- gen_train(size = length(y))

## fit models: lasso + xgboost ----
name_files <- function(seed_model,description) {
  c("lasso","lasso-withgrid","xgboost") %>%
    paste0(., if(!is.null(seed_model))"-s", seed_model, "_", description, ".rds") %>%
    file.path(path_out[c("lasso","lasso","xgb")],.)
}

## bow ----
bow <- readRDS(file.path("output","bow","bow.rds"))
bow <- bow %>% as(.,"sparseMatrix")
description <- "bow"

t <- lapply(seed_model_list, function(seed_model) {
  files <- name_files(seed_model, description)
  fit <- fit_lasso(
    x = bow, y = y, train = train, file_out = files[1],
    description = description, seed = seed_model)
  fit <- fit_lasso(
    x = bow, y = y, train = train, file_out = files[2],
    description = description, seed = seed_model, with_grid = T)
  fit <- fit_xgboost(
    x = bow, y = y, train = train, file_out = files[3],
    description = description, seed = seed_model, no_cores = no_cores)
})
rm(t,description)

## w2v ----
w2v_embedding <- expand.grid(
  dim = c(100,300),
  stat = c("sum","wsum")
)

t <- split(w2v_embedding, seq(nrow(w2v_embedding))) %>%
  lapply(., function(x) {
    xbow <- bow / if(x$stat == "sum") 1 else rowSums(bow)
    
    t <- lapply(seed_w2v_list, function(seed_w2v) {
      file <- paste0("w2v",x$dim,if(!is.null(seed_w2v))"-s",seed_w2v,".bin") %>%
        file.path(".","output","w2v",.)
      if(!file.exists(file)) {
        flog.warn("Insumo ausente: %s", file)
        return(NULL)
      }
      
      xw2v <- file %>% read.vectors(.)
      xw2v <- xw2v[colnames(bow),]
      xw2v <- (xbow %*% xw2v) %>% as.matrix()
      
      description <- paste0("w2v",x$dim,"-",x$stat,
                            if(!is.null(seed_w2v))"-s",seed_w2v)
      
      t <- lapply(seed_model_list, function(seed_model) {
        files <- name_files(seed_model, description)
        fit <- fit_lasso(
          x = xw2v, y = y, train = train, file_out = files[1],
          description = description, seed = seed_model)
        fit <- fit_lasso(
          x = xw2v, y = y, train = train, file_out = files[2],
          description = description, seed = seed_model, with_grid = T)
        fit <- fit_xgboost(
          x = xw2v, y = y, train = train, file_out = files[3],
          description = description, seed = seed_model, no_cores = no_cores)
        return(NULL)
      })
    })
  })

## with weights ----
w2v_embedding <- expand.grid(
  dim = c(100),
  stat = "wxgb"
)

t <- lapply(seed_model_list, function(seed_bow) {
  # importance of each words (xgboost applied to bow)
  fit_bow <- name_files(seed_bow, "bow")[3] %>%
    readRDS(file = .)
  
  t <- split(w2v_embedding, seq(nrow(w2v_embedding))) %>%
    lapply(., function(x) {
      t <- lapply(seed_w2v_list, function(seed_w2v) {
        file <- paste0("w2v",x$dim,if(!is.null(seed_w2v))"-s",seed_w2v,".bin") %>%
          file.path(".","output","w2v",.)
        if(!file.exists(file)) {
          flog.warn("Insumo ausente: %s", file)
          return(NULL)
        }
        
        xw2v <- file %>% read.vectors(.)
        xw2v <- bow_xgweight(
          model = fit_bow$xgb, bow = bow, w2v = xw2v) %>% as.matrix

        description <- paste0("w2v",x$dim,if(!is.null(seed_w2v))"-s",seed_w2v,
                              "-wxgb",if(!is.null(seed_bow))"-s",seed_bow)
        
        t <- lapply(seed_model_list, function(seed_model) {
          files <- name_files(seed_model, description)
          fit <- fit_lasso(
            x = xw2v, y = y, train = train, file_out = files[1],
            description = description, seed = seed_model)
          fit <- fit_xgboost(
            x = xw2v, y = y, train = train, file_out = files[3],
            description = description, seed = seed_model, no_cores = no_cores)
          return(NULL)
        })
      })
    })
})
