## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
cases <- file.path(".","output","track_model-bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(model == "xgb") %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("mbow_", x))
cases2 <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x))
cases <- merge(cases, cases2) %>%
  tidyr::crossing(ncores = no_cores, seed = 1:1,
                  ibow = c(T,F), nbow = c(T,F), nweight = c(T,F))
rm(cases2)

# create directories where models will be saved
path_out <- file.path(".","output","model")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

## load data ----
y <- read.table(file = file.path("input","labels_clean.txt"),
                header = FALSE, sep = "\t", stringsAsFactors = FALSE)[[1]]
train <- gen_train(size = length(y))
bow <- readRDS(file.path("output","bow","bow.rds")) %>% as(.,"sparseMatrix")

## fit models: lasso + xgboost ----
t <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    w2v <- case$w2v_file %>% read.vectors(.)
    fit_bow <- readRDS(file = case$mbow_file)
    x <- with(case, bow_xgweight(
      model = fit_bow$xgb, bow = bow, w2v = w2v, indic_bow = ibow,
      norm_bow = nbow, norm_weigh = nweight)) %>% as.matrix()
    
    description <- paste0("w2v",case$w2v_dim,"wxgb")
    models <- c("lasso","xgb")
    files <- paste0(models,"_",description) %>%
      namefile(path = path_out, str = ., extension = ".rds")
    cases <- case %>%
      dplyr::select(ncores,seed,ibow,nbow,nweight,w2v_file,mbow_file) %>%
      tidyr::crossing(model = models) %>%
      dplyr::mutate(file_out = files) 
    file.path(".","output","track_model-wxgb.txt") %>%
      write.table(x = cases, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    fit <- fit_lasso(
      x = x, y = y, train = train, file_out = files[1],
      description = description, seed = case$seed)
    fit <- fit_xgboost(
      x = x, y = y, train = train, file_out = files[2],
      description = description, seed = case$seed, no_cores = case$ncores)
    return(NULL)
  })

## plot roc ----
c1 <- file.path(".","output","track_model-bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(model == "xgb") %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("mbow_", x))
c2 <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x))
c3 <- file.path(".","output","track_model-wxgb.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out)

cases <- merge(c1, c2) %>% merge(., c3)
cases <- lapply(cases$file, file.exists) %>% unlist() %>% cases[.,]
rm(c1,c2,c3)

roc <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    if(case$model == "lasso")
      x <- readRDS(file = case$file)$lasso$roc
    else
      x <- readRDS(file = case$file)$xgb$roc
    if(is.null(x)) return(NULL)
    x[c("sensitivities","specificities")] %>%
      do.call(cbind, .) %>% as.data.frame() %>% cbind(., case)
  }) %>% do.call(rbind, .)

filename <- "roc_wxgb"
ggpath <- file.path(".","plots","models-performance")
roc %>% mutate(test = paste(ibow,nbow,nweight, sep = "-")) %>%
  ggplot(data = ., aes(specificities,sensitivities, col = test)) +
  geom_line() + labs(x = "Especificidade", y = "Sensibilidade") +
  facet_grid(. ~ model)
# ggsave2(filename = filename, path = ggpath)

auc <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    if(case$model == "lasso")
      x <- readRDS(file = case$file)$lasso$roc$auc
    else
      x <- readRDS(file = case$file)$xgb$roc$auc
    if(is.null(x)) return(NULL)
    case %>% mutate(auc = x[[1]])
  }) %>% do.call(rbind, .)

auc %>% dplyr::select(w2v_dim,model,ibow,nbow,nweight,auc) %>% View

