output$select_xy <- renderUI({
  tagList(
    numericInput("x", label="x", value=1, min = 1, max = input$select_dim, step = 1),
    numericInput("y", label="y", value=2, min = 1, max = input$select_dim, step = 1)
  )
})

output$plot <- renderPlotly({
  cases <- cases$sample[1,]
  input$slider1 %>% req()

  y <- label()
  bow <- bow()
  w2v <- w2v()

  if(!input$select_pca) {
    x <- w2v[c(req(input$x),req(input$y))] %>%
      dplyr::rename(.,X=1,Y=2)
  } else {
    x <- w2v %>% dplyr::rename(.,X=V1,Y=V2)
  }
  
  x <- x %>% tibble::rownames_to_column(., var = "words") %>%
    dplyr::mutate(prop = colSums(bow[y,])/colSums(bow)) %>%
    dplyr::filter(prop < input$slider1[1] | prop > input$slider1[2])

  gg <- ggplot(x, aes(X, Y, col = prop)) + geom_point()
  ggplotly(gg) %>%
    style(p = ., text = x$words)
})
