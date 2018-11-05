## dependencias ----
source(file.path("R","dependencies.R"))

## fit word2vec to reviews ----
path_out <- file.path(".","output","w2v")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

cases <- expand.grid(
  ncores = no_cores,
  dim = c(2,50,100,300),
  seed = 1:1,
  window = 12,
  iter = 10,
  negsamples = 0,
  mincount = 1000
)
cases_rm <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_out))
rm(cases_rm)

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
rm(cases)

## generate represation matrix per review ----
path_out <- file.path(".","output","text-w2v")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

cases_w2v <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x))
cases_rm <- file.path(".","output","track_text-w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F)

data <- file.path("input","reviews_clean.txt") %>%
  read.table(., header = F, sep = "\t", stringsAsFactors = F)

r <- cases_w2v %>% split(., seq(nrow(.))) %>%
  lapply(., function(case) {
    w2v <- case$w2v_file %>% read.vectors(.)
    
    text <- cases_rm %>%
      dplyr::filter(w2v_file == case$w2v_file) %>%
      dplyr::select(text) %>% unlist(.) %>%
      setdiff(seq(nrow(data)), .) 

    r <- text %>%
      purrr::map(., function(idx) {
        file <- paste0("w2v",case$w2v_dim,"_text",idx) %>%
          namefile(path = path_out, str = ., extension = ".bin")
        case <- data.frame(text = idx, w2v_file = case$w2v_file, file_out = file)
        file.path(".","output","track_text-w2v.txt") %>%
          write.table(x = case, file = ., append = file.exists(.),
                      col.names = !file.exists(.), row.names = F)
        
        words <- data[idx,] %>% str_split(., "[[:space:]]")
        w2v[match(words[[1]], row.names(w2v), nomatch = 0),] %>%
          write.binary.word2vec(., filename = file)
      })
    NULL
  })
rm(cases_rm,cases_w2v)

# tt <- file.path(".","output","track_text-w2v.txt") %>%
#   read.table(., header = T, stringsAsFactors = F) %>%
#   head(1) %>%
#   read.vectors(filename = as.character(.$file_out))
