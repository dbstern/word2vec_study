output$select_w2v_ts <- renderUI({
  max <- ifelse(input$select_rdim == "tsne", 2, input$select_dim)
  tagList(
    numericInput("y", label="dim", value=1, min = 1, max = max, step = 1),
    numericInput("text", label="Text", value=1, min = 1, max = length(req(label())), step = 1)
  )
})

output$plot <- renderPlotly({
  w2v <- w2v()
  y <- label()[req(input$text)]
  
  idx_words <- file.path("..","input","reviews_clean.txt") %>%
    read.table(., header = F, sep = "\t", stringsAsFactors = F,
               nrows = 1, skip = input$text-1) %>% "[["(1) %>%
    str_split(., "[[:space:]]") %>% "[["(1) %>%
    match(., row.names(w2v), nomatch = 0)
  w2v <- w2v[input$y] %>%
    dplyr::rename(., dim=1) %>%
    tibble::rownames_to_column(., var = "words") %>%
    dplyr::slice(., idx_words) %>%
    mutate(t = dplyr::row_number())
  
  gg <- ggplot(w2v, aes(t,dim)) +
    geom_line() + geom_point() +
    labs(title = paste("Score:",y))
  ggplotly(gg) %>%
    style(p = ., text = w2v$word)
})
