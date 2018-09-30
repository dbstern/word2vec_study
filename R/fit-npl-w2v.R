## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
path_out <- file.path(".","output","w2v")
if(!dir.exists(path_out))
  dir.create(path_out, recursive = T)

cases <- expand.grid(
  ncores = no_cores,
  dim = c(100,300),
  seed = 1:1,
  window = 12,
  iter = 10,
  negsamples = 0,
  mincount = 1000
)

## gerar os modelos word2vec ----
x <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    
    file_in <- file.path(".","input","reviews_clean.txt")
    case$file_out <- format(Sys.time(), "%s") %>%
      paste0("w2v",case$dim,"_",.,".bin") %>%
      file.path(path_out,.)
    
    flog.info(paste("Calcular w2v:",case$file_out))
    file.path(".","output","track_w2v.txt") %>%
      write.table(x = case, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    set.seed(seed = case$seed)
    w2v100 <- with(case, train_word2vec(
      train_file = file_in, output_file = file_out,
      vectors = dim, threads = ncores, window = window,
      iter = iter, negative_samples = negsamples,
      min_count = mincount
    ))
    return(NULL)
})

