## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## list cases ----
cases_w2v_dim <- get_relpath("w2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::pull(., dim) %>% unique(.) %>% max(.)
cases <- expand.grid(
  rmstopw = rmstopw,
  ncores = no_cores,
  dim = c(cases_w2v_dim,if(db!="amzn")400),
  seed = 1:1,
  window = 12,
  iter = 10,
  negsamples = 0,
  mincount = 1000
)

if(file.exists(get_relpath("d2v",NULL))) {
  cases_rm <- get_relpath("d2v",NULL) %>%
    read.table(., header = T, stringsAsFactors = F)
  cases <- merge(cases, cases_rm, all = T) %>%
    dplyr::filter(., is.na(file_d2v))
  rm(cases_rm)
}
rm(cases_w2v_dim)

## fit doc2vec to reviews ----
oldfile <- get_relpath("txt",T)
newfile <- stringr::str_replace(
  string = oldfile, pattern = "RDS", replacement = "txt")
if(!file.exists(newfile)) {
  data <- load_txt()
  writeLines(text = data, con = newfile)
  rm(data)
}
rm(oldfile)

x <- cases %>%
  split(., seq(nrow(.))) %>%
  lapply(., function(case) {
    file <- get_relpath("d2v",T)
    case$file_d2v <- basename(file)
    get_relpath("d2v",NULL) %>%
      write.table(x = case, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    
    # str <- sprintf("python ./scripts/_old_2d_fit-npl-d2v.py %i %i %i %i %s %s %s",
    #                case$dim, case$window, case$mincount, case$dm, newfile, file, logpath)
    str <- sprintf("python ./scripts/2d_fit-npl-d2v.py %i %i %i %s %s %s",
                   case$dim, case$window, case$mincount, newfile, file, logpath)
    system(str)
  })
rm(x,newfile)

## ----
# library(textTinyR)
# library(fastTextR)
