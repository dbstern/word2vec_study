output$select_w2v <- renderUI({
  cases <- dbcases()
  tagList(
    selectInput("select_plot", label = "Plot type", choices = list("W2V Dispersion"="plot_w2v_disp", "W2V Temp"="plot_w2v_ts"), selected = "plot_w2v_disp"),
    dblabels() %>%
      dplyr::filter(., type=="w2v" | opt%in%c("sample_igeq","dred_w2v")) %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(dbl) {
        track <- ifelse(dbl$type=="model","mw2v",dbl$type)
        selectInput(paste0("select_",dbl$opt), label = dbl$label,
                    choices = sort(unique(cases[[track]][[dbl$opt]])),
                    selected = dbl$selected)
      }),
    uiOutput("select_w2v_cond")
  )
})
output$select_w2v_cond <- renderUI({
  cases <- dbcases()

  x <- list()
  if(input$select_plot == "plot_w2v_disp") {
    y <- checkboxInput("select_filter", label = "Filter by Dispersion", value = F)
    x <- append(x,list(y))
    y <- sliderInput("slider1", label = NULL, min = 0, max = 1, value = c(.40,.60))
    x <- append(x,list(y))
    if(input$select_dred_w2v != "tsne") {
      y <- numericInput("x", label="x", value=1, min = 1, max = input$select_dim, step = 1)
      x <- append(x,list(y))
      y <- numericInput("y", label="y", value=2, min = 1, max = input$select_dim, step = 1)
      x <- append(x,list(y))
    }
  } else {
    max <- ifelse(input$select_dred_w2v == "tsne", 2, input$select_dim)
    y <- numericInput("y", label="dim", value=1, min = 1, max = max, step = 1)
    x <- append(x,list(y))
    y <- numericInput("text", label="Text", value=1, min = 1, max = length(load_label()), step = 1)
    x <- append(x,list(y))
  }
  
  tagList( x )
})

y <- reactive({
  y <- load_label(indic_geq = req(input$select_sample_igeq))
  if(any(y == "neg")) {
    idx <- (y=="neg")
    y[idx] <- F
    y[!idx] <- T
    y <- as.logical(y)
  }
  return(y)
})
w2v <- reactive({
  w2v <- dbcases()$w2v %>%
    dplyr::filter(dim == req(input$select_dim)) %>%
    dplyr::filter(rmstopw == req(input$select_rmstopw)) %>%
    dplyr::pull(., file_w2v) %>%
    wordVectors::read.vectors(.)

  # browser()
  w2v <- get_w2v_dred(w2v, input$select_dred_w2v)
  words <- match(colnames(bow()), row.names(w2v), nomatch = 0)
  w2v <- w2v[words,]

  words <- match(row.names(w2v), colnames(bow()), nomatch = 0)
  bow <- bow()[,words]

  w2v <- data.frame(w2v@.Data) %>%
    tibble::rownames_to_column(., var = "words") %>%
    dplyr::mutate(., prop = colSums(bow[y(),])/colSums(bow))
  return(w2v)
})

output$gg_w2v <- renderPlotly({
  w2v <- w2v()

  if(input$select_plot == "plot_w2v_disp") {
    if(input$select_dred_w2v == "tsne") {
      x <- w2v %>% dplyr::rename(.,X=X1,Y=X2)
    } else {
      vars <- paste0("X",c(input$x,input$y))
      x <- w2v[c(vars,"prop","words")] %>%
        dplyr::rename(.,X=!!names(.[1]),Y=!!names(.[2]))
    }
    
    if(input$select_filter)
      x <- x %>% dplyr::filter(prop < input$slider1[1] | prop > input$slider1[2])
    
    gg <- ggplot(x, aes(X, Y, col = prop)) + geom_point()
    ggplotly(gg) %>% style(p = ., text = x$words)
  } else {
    idx_words <- load_txt()[req(input$text)] %>%
      str_split(., "[[:space:]]") %>% "[["(1) %>%
      match(., w2v$words, nomatch = 0)
    w2v <- w2v[c("words",paste0("X",input$y))] %>%
      dplyr::rename(., dim=2) %>%
      dplyr::slice(., idx_words) %>%
      mutate(position = dplyr::row_number())

    gg <- ggplot(w2v, aes(position,dim)) +
      geom_line() + geom_point()
    if(input$db == "proc")
      gg <- gg + labs(title = paste("Improcedente:",y()[input$text]))
    ggplotly(gg) %>% style(p = ., text = w2v$word)
    
  }
})
