function(input,output,session) {
  observeEvent(input$select_rmstopw, { rmstopw <<- input$select_rmstopw })
  dbcases <- eventReactive(list(rmstopw,input$db), {
    db <<- input$db
    basepath <<- file.path(outpath, input$db)
    x <- get_relpath(NULL)
    cases <- as.list(x) %>%
      purrr::map(., function(file) {
        if(!file.exists(file)) return(NULL)
        type <- basename(file) %>%
          stringi::stri_replace_all(str = ., regex = "(track_)|(.txt)","")
        x <- file %>% read.table(., header = T, stringsAsFactors = F)
        idx <- which(str_detect(names(x), pattern = "file_"))
        for(ii in idx) {
          typeii <- str_remove(names(x)[ii], pattern = "file_")
          typeii <- ifelse(typeii=="model",type,typeii)
          x[ii] <- ifelse(x[[ii]]=="NA","NA",file.path(dirname(file),typeii,x[[ii]]))
        }
        if("stat" %in% names(x)) {
          levels <- c("bow","mean","mean+sd","mean+sd+skew","mean+sd+skew+kurt",
                      # "mean+sd+skew+kurt+quart",
                      "mean+sd+skew+kurt+tailquant",
                      # "mean+sd+skew+kurt+quart+tailquant",
                      "unifquant","mixquant",
                      "mean+sd+skew+kurt+quant",
                      # "pca_dimx10",
                      "pca_dimx20","dm","d2v")
          x <- x %>% dplyr::mutate(., stat = ifelse(stat=="dm+dbow","d2v",stat)) %>%
            dplyr::filter(., stat %in% levels) %>%
            dplyr::mutate(., stat = factor(stat, levels = levels))
        }
        return(x)
      })
    cases
  })
  dblabels <- eventReactive(input$db, { cases_opts[cases_opts[[input$db]],] })
  
  dbcases_selected <- reactive({
    cases <- dbcases()
    
    cases_sample <- cases$sample
    if("sample_prob" %in% names(cases_sample))
      cases_sample <- cases_sample %>% 
      dplyr::filter(sample_prob %in% input$select_sample_prob)
    if("sample_igeq" %in% names(cases_sample))
      cases_sample <- cases_sample %>%
      dplyr::filter(sample_igeq %in% input$select_sample_igeq)

    if(length(input$select_bow) > 0) {
      cases_bow <- cases$bow %>%
        dplyr::filter(., is.na(k) | ("bow_cl" %in% input$select_bow)) %>%
        dplyr::filter(., rmstopw %in% input$select_rmstopw)
      cases_mbow <- cases$mbow %>%
        dplyr::filter(model %in% input$select_model) %>%
        dplyr::inner_join(.,cases_sample) %>%
        dplyr::inner_join(.,cases_bow) %>%
        dplyr::mutate(., stat=as.factor("bow"),dim ="bow",dred_w2v=F,return=F)
      if("modbow" %in% names(cases$mbow))
        cases_mbow <- cases_mbow %>%
        dplyr::filter(modbow %in% input$select_modbow)
      if("label" %in% names(cases$mbow))
        cases_mbow <- cases_mbow %>%
        dplyr::filter(label %in% input$select_label)
    }
    
    cases_w2v <- cases$w2v %>%
      dplyr::filter(., dim %in% input$select_dim) %>%
      dplyr::filter(., rmstopw %in% input$select_rmstopw)
    cases_mw2v <- cases$mw2v %>%
      dplyr::filter(
        model %in% input$select_model &
          dred_w2v %in% input$select_dred_w2v &
          return %in% input$select_return &
          stat %in% input$select_stat
      ) %>%
      dplyr::inner_join(.,cases_sample) #%>%
    
    cases_mw2v <- cases_mw2v %>%
      dplyr::inner_join(.,cases_w2v)
    if("modbow" %in% names(cases$mw2v))
      cases_mw2v <- cases_mw2v %>%
      dplyr::filter(modbow %in% input$select_modbow)
    if("label" %in% names(cases$mw2v))
      cases_mw2v <- cases_mw2v %>%
      dplyr::filter(label %in% input$select_label)
    if("d2v" %in% input$select_stat) {
      cases_d2v <- cases$d2v %>%
        dplyr::filter(., dim %in% input$select_dimd2v &
                        rmstopw %in% input$select_rmstopw)
      cases_md2v <- cases$md2v %>%
        dplyr::filter(
          model %in% input$select_model &
            stat %in% input$select_stat
        ) %>%
        dplyr::inner_join(.,cases_sample) %>%
        dplyr::inner_join(.,cases_d2v)
      cases_mw2v <- merge(cases_mw2v,cases_md2v,all=T) %>%
        dplyr::mutate(., stat = factor(stat, levels = levels(cases_mw2v$stat)))
    }

    cases <- if(length(input$select_bow) > 0) {
      merge(cases_mbow,cases_mw2v,all=T) %>%
        dplyr::mutate(., stat = factor(stat, levels = levels(cases_mw2v$stat)))
    } else { cases_mw2v }
    
    txt_wrap <- function(x)
      str_replace_all(x, "[+]", "+ ") %>%
      str_wrap(string = ., width = 15)
    cases <- cases %>% dplyr::mutate(., stat = factor(txt_wrap(stat), levels = txt_wrap(levels(stat))))
    return(cases)
  })  
  
  bow <- reactive({
    file <- dbcases()$bow %>%
      dplyr::filter(.,is.na(k)) %>%
      dplyr::filter(rmstopw == req(input$select_rmstopw)) %>%
      dplyr::pull(., file_bow)
    validate(need(file.exists(file),
                  "Select models that have been previously adjusted."))
    file %>%
      readRDS(.) %>%
      as(.,"sparseMatrix")
  })
  
  output$container <- renderUI({
    req(input$db)
    if (is.null(input$tab)) return(NULL)
    if (grepl("\\W", input$tab)) return(NULL)
    file.path(".","ui", paste0(input$tab, ".R")) %>%
      source(., local=TRUE) %>% magrittr::extract2("value")
  })
  
  for (file in list.files("server")) {
    source(file.path("server", file), local = TRUE)
  }
}