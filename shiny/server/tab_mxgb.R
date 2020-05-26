get_cols <- function(stat, file, w2v_dim) {
  if(stat == "bow")
    return(colnames(bow()))
  if(stringr::str_detect(stat,"pca")) {
    return(gen_pca_colnames(stat=stat, w2v_dim=w2v_dim))
    # K <- as.numeric(stringr::str_remove(stat,"pca_"))
    # return(paste0("PCA",sprintf("%03d",seq(K))))
  } else if(stat == "d2v") {
    paste0("%0",nchar(w2v_dim),"d") %>%
      sprintf(.,seq(w2v_dim)) %>% paste0("d2v",.)
  } else {
    cols <- get_stw2v_cols(stat, file)
    return(names(cols[cols]))
  }
}

format_table <- function(x, models) {
  dt <- datatable(
    x, rownames = FALSE, options = list(pageLength = 50, dom = "tipr")
  )
  
  if("xgb" %in% models)
    dt <- dt %>% formatPercentage(columns = "XGB Gain", digits = 2) %>%
      formatStyle("XGB Gain",
                  background = styleColorBar(range(0,x[which(names(x)=="XGB Gain")]), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  if("lasso" %in% models)
    dt <- dt %>% formatRound(columns = "Lasso |Coef|", digits = 2) %>%
      formatStyle("Lasso |Coef|",
                  background = styleColorBar(range(0,x[which(names(x)=="Lasso |Coef|")]), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  return(dt)
}

output$select_mxgb <- renderUI({
  cases <- dbcases()

  tagList(
    dblabels() %>%
      dplyr::filter(., opt == "model") %>%
      selectInput2(., multiple = T, cases=cases),
    checkboxGroupInput("select_bow", "", choices = c("bow"="BOW")),
    dblabels() %>%
      dplyr::filter(., (type == "sample") | (opt == "rmstopw")) %>%
      split(., seq(nrow(.)), drop = T) %>%
      purrr::discard(., ~nrow(.) == 0) %>%
      lapply(., function(dbl) selectInput2(dbl, multiple = F, cases=cases)),
    conditionalPanel(
      condition = "input.select_bow == false",
      dblabels() %>%
        dplyr::filter(., (type %in% c("model","w2v")) & 
                        !(opt %in% c("model","rmstopw"))) %>%
        split(., seq(nrow(.))) %>%
        purrr::discard(., ~nrow(.) == 0) %>%
        lapply(., function(dbl) selectInput2(dbl, multiple = F, cases=cases))
    )
  )
})

xgb_cases <- reactive({
  dbcases_selected()
})

mxgb_imp <- reactive({
  print(xgb_cases())
  validate(need(nrow(xgb_cases()) > 0,
                "Select models that have been previously adjusted."))
  xgb_cases() %>%
    split(., seq(nrow(.))) %>%
    lapply(., function(case) {
      file <- case$file_model
      if(!file.exists(file)) return(NULL)
      data <- readRDS(file)

      if(case$model == "xgb") {
        if(is.null(data$xgb$feature_names)){
          cols <- get_cols(case$stat, merge(case,dbcases()$stw2v)$file_stw2v, w2v_dim=case$dim)
          x <- xgboost::xgb.importance(feature_names = cols , model = data$xgb)
        } else {
          x <- xgboost::xgb.importance(feature_names = data$xgb$feature_names, model = data$xgb)
        }
        x <- x %>% as.data.frame(.) %>% dplyr::select(.,Feature,value=Gain)
      } else {
        cols <- get_cols(case$stat, merge(case,dbcases()$stw2v)$file_stw2v, w2v_dim=case$dim)
        cols <- c("(Intercept)",cols)
        x <- coef(data$lasso)
        x <- data.frame(
          # Feature = x@Dimnames[[1]][x@i+1],
          # value = abs(x@x)
          Feature = cols[x@i+1],
          value = abs(x@x)
        ) %>% dplyr::filter(., Feature!="(Intercept)")
      }

      x <- x %>% dplyr::mutate(., stat = case$stat, model = case$model) %>%
        dplyr::mutate(., model = ifelse(model=="xgb","XGB Gain","Lasso |Coef|"))

      if(str_detect(case$stat, pattern = "pca")) {
        x <- x %>% mutate(stat = Feature)
      } else {
        levels <- c("mean","sd","skew","kurt","bow",
                    paste0("quant",sprintf("%03d",seq(0,100))),"Other")
        
        x <- x %>% mutate(
          ., stat = ifelse(
            str_detect(Feature, pattern = "_"),
            sub(x = Feature, pattern = "_.*", replacement = ""), "bow"),
          statdim = ifelse(
            str_detect(Feature, pattern = "_"),
            sub(x = Feature, pattern = ".*_", replacement = ""), "bow")
        )
      }
      
      x %>% as.data.frame(.)
    })
})

mxgb_group_imp <- reactive({
  stat <- input$select_stat
  if(stringr::str_detect(stat,"pca")) {
    levels <- get_cols(stat=stat, w2v_dim=unique(xgb_cases()$dim))
    levels <- c(levels,"Other")
  } else {
    levels <- c("mean","sd","skew","kurt","bow",
                paste0("quant",sprintf("%03d",seq(0,100))),"Other")
  }

  x <- mxgb_imp() %>%
    lapply(., function(x) {
      if(is.null(x)) return(NULL)
      top <- 19
      x <- x %>%
        group_by(model,stat) %>%
        summarise(., value = sum(value)) %>%
        arrange(., desc(value)) %>%
        group_by(model, stat = c(stat[seq(min(n(),top))],
                                 rep("Other", max(0,n()-top)))) %>%
        summarise(., value = sum(value)) %>%
        ungroup(.)
      x
    }) %>% do.call(rbind,.) %>%
    mutate(., stat = factor(stat, levels = rev(levels))[,drop=T])
})

mxgb_group_imp_nonzero <- reactive({
  stat <- input$select_stat
  if(stringr::str_detect(stat,"pca")) {
    K <- as.numeric(stringr::str_remove(stat,"pca_"))
    levels <- c(paste0("PCA",sprintf("%03d",seq(K))),"Other")
  } else {
    levels <- c("mean","sd","skew","kurt","bow",
                paste0("quant",sprintf("%03d",seq(0,100))),"Other")
  }
  
  x <- mxgb_imp() %>%
    lapply(., function(x) {
      if(is.null(x)) return(NULL)
      top <- 20
      x <- x %>%
        group_by(model,stat) %>%
        summarise(., n = n()) %>%
        arrange(., desc(n)) %>%
        group_by(model, stat = c(stat[seq(min(n(),top))],
                                 rep("Other", max(0,n()-top)))) %>%
        summarise(., n = sum(n)) %>%
        ungroup(.)
      x
    }) %>% do.call(rbind,.) %>%
    mutate(., stat = factor(stat, levels = rev(levels))[,drop=T])
})

output$gg_mxgb_imp <- renderPlotly({
  x <- mxgb_group_imp()
  gg <- ggplot(x, aes(x = stat, y = value)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = round(value,2)), vjust=1.6,
              position = position_dodge(0.9), size=3.5) +
    theme(axis.title.y = element_blank()) +
    coord_flip()
  if(length(input$select_model) > 1) {
    gg <- gg + facet_grid(. ~ model, scales = "free_x") +
      theme(axis.title = element_blank())
  } else {
    gg <- gg + labs(y = x$model[1])
  }
  plotly::ggplotly(gg)
})
output$gg_mxgb_imp_nonzero <- renderPlotly({
  x <- mxgb_group_imp_nonzero()
  gg <- dplyr::mutate(x, caption = n, n = ifelse(stat=="Other",0,n)) %>%
    # dplyr::filter(x, stat != "Other") %>%
    ggplot(., aes(x = reorder(stat,n), y = n, label = caption)) +
    geom_col(fill = "lightblue") +
    geom_text(size = 3, vjust = 0.5) +
    theme(axis.title.y = element_blank()) +
    # geom_text(data=other, aes(x=1.8, y=-1, label=caption), 
    #           colour="black", inherit.aes=FALSE, parse=FALSE)
    labs(y = "# Non-Zero Coefficients", caption = "caption") +
    coord_flip()
  if(length(input$select_model) > 1)
    gg <- gg + facet_grid(. ~ model, scales = "free_x")
  
  gg
})

output$dt_mxgb_imp_ <- DT::renderDataTable({
  x <- mxgb_group_imp() %>%
    tidyr::spread(model, value, fill = 0)
  format_table(x, input$select_model)
})

output$dt_mxgb_imp <- DT::renderDataTable({
  x <- mxgb_imp() %>%
    do.call(rbind, .) %>%
    dplyr::select(., -starts_with("stat")) %>%
    tidyr::spread(model, value, fill = 0)
  format_table(x, input$select_model)
})

output$mxgb_export <- downloadHandler(
  filename = function() {
    paste0("mxgb_",xgb_cases()$stat,".html")
  },
  content = function(file) {
    src <- normalizePath('mxgb_export.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'mxgb_export.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    out <- render('mxgb_export.Rmd', html_document())
    file.rename(out, file)
  }
)
