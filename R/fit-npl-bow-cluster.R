## dependencias ----
source(file.path("R","dependencies.R"))

## global ----
path_out <- file.path(".","output","bow")

## apply cluster to bow ----
cases <- data.frame(
  cluster = "hclust", method = "ward.D2",
  k = seq(from = 50, to = ncol(bow), by = 50), stringsAsFactors = F
)
cases_rm <- file.path(".","output","track_bow.txt") %>%
  read.table(., header = T, stringsAsFactors = F)
cases <- merge(cases, cases_rm, all = T) %>%
  dplyr::filter(., is.na(file_out))
rm(cases_rm)

bow <- readRDS(file.path(path_out,"bow.rds"))
hc <- file.path(".","output","cluster") %>%
  dir(., pattern = "^hclust", full.names = T) %>%
  readRDS(file = .) %>%
  magrittr::extract2("ward.D2")

r <- split(cases, seq(nrow(cases))) %>%
  lapply(., function(case) {
    groups <- cutree(tree = hc, k = case$k)
    bow <- lapply(seq_len(case$k), function(g) {
      Matrix::rowSums(bow[,groups == g])
    }) %>%
      do.call(cbind, .) %>%
      as(., "sparseMatrix")
    
    case$file_out <- namefile(path = path_out, str = "bow-cl", extension = ".rds")
    dirname(path_out) %>% 
      file.path(., "track_bow.txt") %>%
      write.table(x = case, file = ., append = file.exists(.),
                  col.names = !file.exists(.), row.names = F)
    saveRDS(object = bow, file = case$file_out)
  })


