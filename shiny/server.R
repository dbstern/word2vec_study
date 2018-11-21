function(input, output) {
  label <- eventReactive(input$indic_geq, {
    load_labels(indic_geq = input$indic_geq)
  }, ignoreNULL = FALSE)
  bow <- reactive({
    cases$bow %>%
      dplyr::filter(.,is.na(bow_cluster)) %>%
      .$bow_file_out %>%
      file.path("..",.) %>%
      readRDS(.) %>%
      as(.,"sparseMatrix")
  })
  w2v <- eventReactive({
    input$select_dim
    input$select_pca
  }, {
    bow <- req(bow())
    w2v <- cases$w2v %>%
      dplyr::filter(w2v_dim == input$select_dim) %>%
      .$w2v_file_out %>%
      file.path("..",.) %>%
      read.vectors(.) %>%
      as.matrix()
    w2v <- w2v[colnames(bow),]
    
    if(input$select_pca) {
      x <- Rtsne(w2v, perplexity = 50, pca = FALSE)$Y
      row.names(x) <- row.names(w2v)
      w2v <- x %>% as.data.frame(.)
    } else {
      w2v <- w2v@.Data %>% as.data.frame(.)
    }
    
    return(w2v)
  })
  
  output$selectors <- renderUI({
    if (is.null(input$select_plot)) return(NULL)
    if (grepl("\\W", input$select_plot)) return(NULL)
    file.path("ui", paste0(input$select_plot, ".R")) %>%
      source(., local = TRUE)
  })
  observeEvent(
    input$select_plot,
    file.path("server", paste0(input$select_plot, ".R")) %>%
      source(., local = TRUE)
  )
}