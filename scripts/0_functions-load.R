# db/basepath are defined at '0_dependencies.R'

get_relpath <- function(type, file) {
  x <- list(
    train = file.path("input","train.RDS"),
    txt = file.path("input",paste0("txt_clean",if(rmstopw)"_rmstopw",".RDS")),
    label = file.path("input","labels.RDS"),
    sample = file.path("output",".","."),
    bow = file.path("output","bow",paste0("bow",if(rmstopw)"_rmstopw",".rds")),
    w2v = file.path("output","w2v","w2v_%s.bin"),
    clw2v = file.path("output","clw2v","clw2v_%s.rds"),
    clbow = file.path("output","bow","clbow_%s.rds"),
    d2v = file.path("output","d2v","d2v_%s.txt"),
    mbow = file.path("output","mbow","model_%s.rds"),
    stw2v = file.path("output","stw2v","stw2v_%s.txt"),
    mw2v = file.path("output","mw2v","model_%s.rds"),
    md2v = file.path("output","md2v","model_%s.rds")
  ) %>% purrr::map(., function(x) file.path(basepath,x))
  
  if(!is.null(type)) {
    x <- x[[type]]
  } else {
    x <- sapply(names(x)[-c(1:3)], function(type)
      file.path(dirname(dirname(x[[type]])),paste0("track_",type,".txt")),
      USE.NAMES = T, simplify = F)
    return(x)
  }
  
  if(is.null(file)) {
    x <- file.path(dirname(dirname(x)),paste0("track_",type,".txt"))
  } else if(is.character(file)) {
    x <- file.path(dirname(x),file)
  } else if(!file) {
    x <- dirname(x)
    if(!dir.exists(x))
      dir.create(x, recursive = T)
  } else {
    x <- sprintf(x,format(Sys.time(), "%s"))
    if(!dir.exists(dirname(x)))
      dir.create(dirname(x), recursive = T)
  }
  return(x)
}

download_db <- function() {
  relpath <- dirname(get_relpath("txt",T))
  if(stringr::str_detect(db,"nltk[.]")) {
    file <- file.path(relpath,"dados.txt")
    if(file.exists(file)) {
      flog.info(paste(db,"has already been downloaded"))
      return(NULL)
    }
    
    dbpckg <- stringr::str_split(db,"[.]",simplify = T)
    pyscript <- sprintf("./scripts/download-%s.py",dbpckg[1])
    str <- sprintf("python %s %s %s %s", pyscript, dbpckg[2], relpath, file)
    pipe(str,"w")
  } else if(db == "imdb") {
    files <- file.path(relpath,paste0("imdb_",c("test","train"),".rds"))
    if(all(file.exists(files))) {
      flog.info(paste(db,"has already been downloaded"))
      return(NULL)
    }
    
    x <- textdata::dataset_imdb(dir = relpath, clean = T)
    textdata::dataset_imdb(dir = relpath, return_path = T) %>%
      dir(path = ., pattern = "rds", full.names = T)  %>%
      file.rename(from = ., to = file.path(relpath, basename(.)))
    textdata::dataset_imdb(dir = relpath, delete = T)
  }
  
  x <- list(
    rcv1 = function() {
      # if(!dir.exists(relpath)) dir.create(relpath, recursive = T)
      # 
      # url <- "http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a12-token-files/lyrl2004-non-v2_tokens_%s.dat.gz"
      # url <- sprintf(url, c(paste0("test_pt",0:3),"train"))
      # 
      # for (idx in seq(url)) {
      #   filegz <- file.path(relpath,basename(url[idx]))
      #   if(!file.exists(filegz)) download.file(url[idx],filegz)
      #   
      #   filerds <- file.path(relpath,paste0("txt_raw-",idx,".txt"))
      #   dat <- gzfile(filegz)
      #   readLines(dat,10)
      #   # [1] ".I 26676"                                                                                                                                                                                                                        
      #   # [2] ".W"                                                                                                                                                                                                                              
      #   # [3] "sunday sunday fine fine march big week week group left unit million million pound pound includ own own own late build time time busi busi busi busi robert robert robert"                                                        
      #   # [4] "flem flem flem flem flem flem flem flem flem flem flem shift shift shift invest invest invest invest invest invest made bank bank personnel personnel personnel billion jardin jardin jardin jardin"                             
      #   # [5] "jardin jardin early jfim spokesm spokesm spokesm spokesm strateg expert expert departur departur year order compens irregul involut promot account trust trust trust first meant telegraph newspap highlight engag damag limitat"
      #   # [6] "exercis prob month expos allocat joint matheson privat regulat regulat regulat percent manag manag manag manag manag eastern move move move move move london london london london london london armstrong armstrong"             
      #   # [7] "armstrong report april part part run run staff sens colin front secur kong kong kong kong market asian asian trad trad indian brit react reason reason reason reason reason organ commit"                                        
      #   # [8] "person fund fund fund fund fund fund fund fund remain hong hong hong hong futur"                                                                                                                                                 
      #   # [9] ""                                                                                                                                                                                                                                
      #   # [10] ".I 26701"                                         
      # }
    }
  )
}

## help functions ----
gen_pca_colnames <- function(K=NULL, stat=NULL, w2v_dim=NULL) {
  if(is.null(K))
    K <- w2v_dim * as.numeric(stringr::str_remove(stat,"pca_dimx"))
  paste0("%0",nchar(K),"d") %>% sprintf(.,seq(K)) %>% paste0("PCA",.)
}
get_stw2v_cols <- function(stat, file) {
  regex_quant <- list(
    "tailquant" = c(0:5,95:100),
    "quart" = 1:3*25,
    "unifquant" = 0:20*5,
    "mixquant" = c(c(0:5*2,3:10*5),100-rev(c(0:5*2,3:10*5)))
  )
  pattern <- paste0("([+]",names(regex_quant),")",collapse="|")
  replace_all <- stringr::str_detect(string = stat, pattern = names(regex_quant)) %>%
    regex_quant[.] %>% unlist(.) %>% unique(.) %>% sort(.) %>%
    sprintf("(%03d)", .) %>% paste(.,collapse="|") %>% paste0("+quant(", ., ")_")
  stat <- stringr::str_replace_all(stat, pattern=pattern, replacement="") %>%
    paste0(., replace_all) %>%
    stringr::str_split(., pattern="\\+") %>% "[["(1) %>%
    paste0(.,collapse = "|")
  
  # stat <- stat %>%
  #   stringr::str_replace(., pattern="tailquant", replacement="quant((00[0-5])|(09[5-9])|(100))_") %>%
  #   stringr::str_replace(., pattern="quart", replacement="quant0((25)|(50)|(75))_") %>%
  #   stringr::str_split(., pattern="\\+") %>% "[["(1) %>%
  #   paste0(.,collapse = "|")
  cols <- read.table(file = file, header = F, nrows = 1, stringsAsFactors = F)
  which_cols <- cols %>% unlist() %>% str_detect(., stat)
  names(which_cols) <- cols
  return(which_cols)
}

## load files ----
load_raw_data <- function(filter=T) {
  path <- dirname(get_relpath("txt",T))
  if(db == "amzn") {
    x <- file.path(path,"Reviews.csv") %>%
      fread(., header=T) #%>%
    # dplyr::rename(., txt=Text, label=improcedente)
    idx <- match(c("Text","Score"), names(x))
    names(x)[idx] <- c("txt","label")
  } else if(db == "proc") {
    x <- file.path(path,"dados_train.RDS") %>%
      readRDS(file = .) #%>%
    # dplyr::rename(., label=improcedente)
    idx <- str_detect(c("improcedente"), names(x))
    names(x)[idx] <- c("label")
  } else if(db == "ptin") {
    setlabel <- function(label) {
      x <- rep(NA, length(label))
      x[which(label %in% c("negou"))] <- F
      x[which(label %in% c("aceitou","parcial"))] <- T
      return(x)
    }
    x <- file.path(path,"d_sg_com_textos.rds") %>%
      readRDS(file = .) %>%
      dplyr::mutate(., label = setlabel(dec_val))
    if(filter)
      x <- x %>% dplyr::filter(., !is.na(label)) %>%
      dplyr::filter(., part_tipo_litigio == "PF-nPF")
  } else if(stringr::str_detect(db,"nltk[.]")) {
    x <- file.path(path,"dados.txt") %>%
      read.table(., header=T, stringsAsFactors=F)
  } else if(db == "imdb") {
    x_train <- file.path(path,"imdb_train.rds") %>%
      readRDS(file = .) %>%
      dplyr::mutate(., train = T)
    x_test <- file.path(path,"imdb_test.rds") %>%
      readRDS(file = .) %>%
      dplyr::mutate(., train = F)
    x <- dplyr::bind_rows(x_train,x_test) %>%
      dplyr::mutate(., label = (sentiment=="pos")) %>%
      dplyr::select(., train, label, txt = text)
    rm(x_train,x_test)
  }
  return(x)
}

load_txt <- function() {
  x <- get_relpath("txt",T) %>%
    readRDS(file = .)
  if(db == "amzn") {
    return(x[-233939]) # texto em branco
  }
  return(x)
}

load_label <- function(indic_geq=NULL, label=NULL) {
  x <- get_relpath("label",T) %>%
    readRDS(file = .)
  
  if(db == "amzn") {
    x <- x[-233939] # texto em branco
    if(!is.null(indic_geq))
      return(x >= indic_geq)
  }
  if(!is.null(label))
    x <- x[[label]]
  return(x)
}

# load_stw2v_old <- function(file, stat, model) {
#   get_cols <- function() {
#     stat <- stat %>%
#       stringr::str_replace(., pattern="tailquant", replacement="quant((00[0-5])|(09[5-9])|(100))_") %>%
#       stringr::str_split(., pattern="\\+") %>% "[["(1) %>%
#       paste0(.,collapse = "|")
#     cols <- read.table(file = file, header = F, nrows = 1, stringsAsFactors = F) %>%
#       unlist() %>% str_detect(., stat)
#     return(cols)
#   }
#   
#   tailquant <- str_detect(stat, pattern = "(tailquant)")
#   allquant <- str_detect(stat, pattern = "[^(tail)](quant)")
#   libsvmfile <- str_replace(file, pattern = "txt", replacement = "dat")
#   if(file.exists(libsvmfile) & allquant & model == "xgb") {
#     data <- xgb.DMatrix(libsvmfile)
#   } else if(allquant & model == "lasso") {
#     # } else if((tailquant|allquant) & model == "lasso") {
#     data <- bigmemory::read.big.matrix(file, sep = " ", header = T)
#     if(tailquant) {
#       data_colnames <- colnames(data)[get_cols()]
#       data <- bigmemory::deepcopy(data, cols = get_cols())
#       colnames(data) <- data_colnames
#       gc()
#     }
#   } else {
#     cols <- ifelse(get_cols(),"numeric","NULL")
#     data <- read.table(file, header = T, colClasses = cols)
#     data <- as.matrix(data)
#   }
#   
#   return(data)
# }
load_stw2v <- function(file, stat, model, train=NULL, ncores=NULL) {
  if(stringr::str_detect(stat,"pca")) {
    cols <- which(get_stw2v_cols(stat=".", file))
    data <- bigstatsr::big_read(file = file, select = cols)
    x <- get_stw2v_dred(x = data, dred = stat, train, ncores=ncores)
    # colnames(x) <- paste0("%0",nchar(dim(x)[2]),"d") %>%
    #   sprintf(.,seq(dim(x)[2])) %>% paste0("PCA",.)
    colnames(x) <- gen_pca_colnames(K=dim(x)[2])
    unlink(c(data$bk,data$rds));rm(data);gc()
    
    if(model == "lasso") {
      file_bk <- str_replace(file, ".txt", "_")
      x <- as_FBM(x, backingfile = file_bk)
    }
    return(x)
  }

  allquant <- str_detect(stat, pattern = "([+]quant)")
  libsvmfile <- str_replace(file, pattern = "txt", replacement = "dat")
  if(file.exists(libsvmfile) & allquant & model == "xgb") {
    data <- xgb.DMatrix(libsvmfile)
  } else if(model == "lasso") {
    cols <- which(get_stw2v_cols(stat, file))
    data <- big_read(file = file, select = cols)
  } else {
    cols <- ifelse(get_stw2v_cols(stat, file),"numeric","NULL")
    data <- read.table(file, header = T, colClasses = cols)
    data <- as.matrix(data)
  }
  
  return(data)
}

load_d2v <- function(file, dim, stat, model) {
  cols <- if(stat == "dm") {
    rep(c(F,T), each = dim)
  } else {
    rep(T, each = (2*dim))
  }

  if(model == "lasso") {
    # cols <- which(get_stw2v_cols("", file))
    data <- big_read(file = file, select = which(cols))
  } else {
    cols <- ifelse(cols,"numeric","NULL")
    data <- read.table(file, header = F, colClasses = cols)
    data <- as.matrix(data)
  }
  return(data)
}

load_bow <- function(file, model, modbow) {
  txtfile <- str_replace(file, pattern = "rds", replacement = "txt")
  libsvmfile <- str_replace(file, pattern = "rds", replacement = "dat")
  if(file.exists(libsvmfile) & model == "xgb") {
    data <- xgb.DMatrix(libsvmfile)
  } else if(file.exists(txtfile) & model == "lasso") {
    cols <- read.table(file = txtfile, header = F, nrows = 1, stringsAsFactors = F)
    cols <- seq(length(cols))
    data <- big_read(file = txtfile, select = cols)
  } else {
    data <- readRDS(file) %>% as.matrix(.)
    if(!is.null(modbow)) if(modbow)
      data <- data[,-which(str_detect(colnames(data), pattern = "procedente"))]
  }
  
  return(data)
}

## transform data ----
get_w2v_dred <- function(w2v, dred) {
  if(dred == "tsne") {
    x <- Rtsne::Rtsne(w2v, perplexity = 50, pca = FALSE)$Y
    row.names(x) <- row.names(w2v)
    w2v <- x
  } else if(dred == "pca") {
    w2v <- prcomp(w2v, center = TRUE, scale = TRUE)$x
  }
  return(w2v)
}

get_stw2v_dred <- function(x, dred, train, ncores) {
  K <- dim(x)[2]*as.numeric(stringr::str_remove(dred,"pca_dimx"))/105
  rsvd <- big_randomSVD(x, fun.scaling = big_scale(), k = K,
                        ind.row = which(train), ncores = ncores)
  x <- predict(rsvd, x)
  return(x)
}

get_returns <- function(tw2v) {
  tw2v <- if(nrow(tw2v) < 2) {
    rep(0,ncol(tw2v)) %>% matrix(., nrow = 1)
  } else { abs(diff(tw2v)) }
  return(tw2v)
}

## ----
