## dependencias ----
source(file.path("R","dependencies.R"))

colSdColMeans <- function(x, na.rm=TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x)) # thanks @flodel
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
  return((colVar * n/(n-1)))
}

## global ----
cases_w2v <- file.path(".","output","track_w2v.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::select(., everything(), file = file_out) %>%
  dplyr::rename_all(., .funs = function(x) paste0("w2v_", x)) %>%
  dplyr::filter(., w2v_dim == 100)
cases_vtw2v <- tryCatch({
  file.path(".","output","track_var_tw2v.txt") %>%
    read.table(., header = T, stringsAsFactors = F)
} , error = function(e) data.frame(file_out = NA) )
cases <- cases_w2v
cases <- merge(cases,cases_vtw2v,all=T)
rm(cases_w2v,cases_vtw2v)

path_out <- file.path(".","output","var_tw2v")
if(!dir.exists(path_out)) dir.create(path_out, recursive = T)

## load data ----
data <- file.path("input","reviews_clean.txt") %>%
  read.table(., header = F, sep = "\t", stringsAsFactors = F)

## calculate var ----
r <- cases %>%
  split(., seq(nrow(.))) %>%
  lapply(., function(case) {
    w2v <- case$w2v_file %>% read.vectors(.)
    
    if(is.na(case$file_out)) {
      file <- paste0("var_tw2v",case$w2v_dim) %>%
        namefile(path = path_out, str = ., extension = ".txt")
      case <- case %>% dplyr::mutate(file_out = file) %>%
        dplyr::select(w2v_file,file_out)
      file.path(".","output","track_var_tw2v.txt") %>%
        write.table(x = case, file = ., append = file.exists(.),
                    col.names = !file.exists(.), row.names = F)
      cases_rm <- 0
    } else {
      case <- case %>% dplyr::mutate(file_out = file) %>%
        dplyr::select(w2v_file,file_out)
      file <- case$file_out
      cases_rm <- read.table(file)$text
    }
    
    text <- setdiff(seq(nrow(data)), cases_rm)
    r <- text %>%
      purrr::map(., function(idx) {
        words <- data[idx,] %>% str_split(., "[[:space:]]")
        tw2v <- w2v[match(words[[1]], row.names(w2v), nomatch = 0),]
        if(nrow(tw2v) == 0) return(NULL)
        tvar <- colSdColMeans(tw2v) %>% matrix(., nrow = 1) %>%
          as.data.frame(.) %>% dplyr::mutate(text = idx, dim = 100)
        file %>% write.table(x = tvar, file = ., append = file.exists(.),
                             col.names = !file.exists(.), row.names = F)
      })
    
    gc(verbose = F, full = F)
    
    return(NULL)
  })
