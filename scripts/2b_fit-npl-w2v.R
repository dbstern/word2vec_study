## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## list cases ----
cases <- expand.grid(
  rmstopw = rmstopw,
  ncores = no_cores,
  dim = c(2,50,100),#,300
  seed = 1:1,
  window = 12,
  iter = 10,
  negsamples = 0,
  mincount = 1000
)
if(file.exists(get_relpath("w2v",NULL))) {
  cases_rm <- get_relpath("w2v",NULL) %>%
    read.table(., header = T, stringsAsFactors = F)
  cases <- merge(cases, cases_rm, all = T) %>%
    dplyr::filter(., is.na(file_w2v))
  rm(cases_rm)
}

## fit word2vec to reviews ----
if(nrow(cases) > 0) {
  file_in <- tempfile(pattern = "txt_clean", fileext = ".txt")
  load_txt() %>% write(., file = file_in)
}
x <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    file <- get_relpath("w2v",T)
    case$file_w2v <- basename(file)
    
    flog.info(paste("Calcular w2v:",case$file_w2v))
    get_relpath("w2v",NULL) %>%
      write.table(x = case, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    set.seed(seed = case$seed)
    w2v <- with(case, train_word2vec(
      train_file = file_in, output_file = file,
      vectors = dim, threads = ncores, window = window,
      iter = iter, negative_samples = negsamples,
      min_count = mincount
    ))
    return(NULL)
  })
rm(cases,file_in)

