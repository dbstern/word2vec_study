## dependencias ----
source(file.path("scripts","0_dependencies.R"))

## load data ----
bow <- get_relpath("bow",T) %>%
  readRDS(.) %>% as(.,"sparseMatrix")

## list cases clustering ----
cases_w2v <- get_relpath("w2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::filter(., rmstopw == .GlobalEnv$rmstopw)
cases <- data.frame(cluster = "hclust", method = "ward.D2",
                    stringsAsFactors = F) %>%
  tidyr::crossing(., cases_w2v["file_w2v"])
cases_rm <- get_relpath("clw2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_clw2v))
rm(cases_w2v,cases_rm)

## compute clustering ----
x <- cases %>%
  split(., .$file_w2v) %>%
  lapply(., function(cases) {
    w2v <- cases$file_w2v[1] %>% get_relpath("w2v",.) %>%
      read.vectors(.) %>% as.matrix(.)
    words <- match(colnames(bow), row.names(w2v), nomatch = 0)
    w2v <- w2v[words,]
    dw2v <- (1-cosineSimilarity(w2v,w2v)) %>% as.dist(.)
    
    cases %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(case) {
        file <- get_relpath("clw2v",T)
        case$file_clw2v <- basename(file)
        
        ftry({
          if(case$cluster == "hclust") {
            bow_cl <- hclust(d = dw2v, method = case$method)
          }
          saveRDS(object = bow_cl, file = file)

          get_relpath("clw2v",NULL) %>%
            write.table(x = case, file = ., append = file.exists(.),
                        col.names = !file.exists(.), row.names = F)
        })
      })
  })

## list cases clbow ----
cases <- get_relpath("clw2v",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)  %>%
  tidyr::crossing(., k = seq(from = 50, to = ncol(bow), by = 50))
cases_rm <- get_relpath("bow",NULL) %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases_rm, cases, all = T) %>%
  dplyr::filter(., is.na(file_bow)) %>%
  dplyr::select(., names(cases_rm))
rm(cases_rm)

## apply cluster to bow ----
r <- cases %>% split(., .$file_clw2v) %>%
  lapply(., function(cases) {
    hc <- cases$file_clw2v[1] %>%
      get_relpath("clw2v",.) %>% readRDS(file = .)
    bow <- bow[,hc$labels]
    
    cases %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(case) {
        groups <- cutree(tree = hc, k = case$k)
        bow <- lapply(seq_len(case$k), function(g) {
          Matrix::rowSums(bow[,groups == g,drop=F])
        }) %>%
          do.call(cbind, .) %>%
          as(., "sparseMatrix")

        file <- get_relpath("clbow",T)
        case$file_bow <- basename(file)
        get_relpath("bow",NULL) %>%
          write.table(x = case, file = ., append = file.exists(.),
                      col.names = !file.exists(.), row.names = F)
        saveRDS(object = bow, file = file)
      })
  })


