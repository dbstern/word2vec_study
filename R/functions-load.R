cases <- expand.grid(
  igeq = c(4,5),
  seed = 1234,
  prob = c(.5,.0088)
)
cases_rm <- file.path(".","output","track_sample.txt") %>%
  read.table(., header = T, stringsAsFactors = F) %>%
  dplyr::mutate(rm = T)
cases <- merge(cases,cases_rm, all = T) %>%
  dplyr::filter(is.na(rm)) %>%
  mutate(id = as.numeric(row.names(.)) + max(cases_rm$id)) %>%
  dplyr::select(id,igeq,seed,prob)
file.path(".","output","track_sample.txt") %>%
  write.table(x = cases, file = ., append = file.exists(.),
              col.names = !file.exists(.), row.names = F)
rm(cases,cases_rm)

load_labels <- function(indic_geq) {
  raw <- file.path("input","labels_raw.txt") %>%
    read.table(., header = F) %>% "[["(1)
  raw <- raw[-233939] # texto em branco
  raw >= indic_geq
}

