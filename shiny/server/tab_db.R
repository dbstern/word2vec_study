output$select_db_freq <- renderUI({
  cases <- dbcases()
  dblabels <- dblabels() %>%
    dplyr::filter(., opt%in%c("sample_igeq","label"))
  if(nrow(dblabels) == 0) return(NULL)
  tagList(
    dblabels %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(dbl) {
        track <- ifelse(dbl$type=="model","mw2v",dbl$type)
        selectInput(paste0("select_",dbl$opt), label = dbl$label,
                    choices = c("NULL",sort(unique(cases[[track]][[dbl$opt]]))),
                    selected = dbl$selected)
      }),
    if(db == "nltk.reuters") {
      shiny::numericInput("db_filter","Filter freq:", value = 0, min = 0, max = 1, step = .1)
    }
  )
})

db_label_freq <- reactive({
  cases <- dbcases()
  indic_geq <- input$select_sample_igeq
  if(is.character(indic_geq))
    if(indic_geq=="NULL") indic_geq <- NULL
  label <- input$select_label
  if(is.character(label))
    if(label=="NULL") label <- NULL
  label <- load_label(indic_geq = indic_geq, label = label)
  if(input$db %in% c("proc","ptin")) {
    idx <- label
    label[idx] <- "well-founded"
    label[!idx] <- "unfounded"
    label <- as.factor(label)
  }
  return(label)
})

output$gg_db_freq <- renderPlotly({
  dblabel <- db_label_freq()
  if(!is.data.frame(dblabel)) {
    x <- as.data.frame(dblabel) %>%
      group_by(dblabel) %>%
      summarise(n = n(), freq = n()/nrow(.))
    gg <- ggplot(x, aes(x = dblabel, y = freq)) +
      geom_col(fill = "lightblue") +
      geom_text(aes(label = round(freq,2), y = freq-0.03)) +
      labs(x = "Label", y = "Relative Frequency")
  } else {
    f <- input$db_filter %>% c(., 1-.)
    label_freq <- rbind(colMeans(dblabel),1-colMeans(dblabel)) %>%
      cbind(data.frame(label=c(T,F)),.) %>%
      tidyr::gather(., "type", "freq", -label) %>%
      dplyr::mutate(., n = nrow(dblabel)*freq)
    x <- dplyr::filter(label_freq, freq < max(f) & freq > min(f))
    gg <- ggplot(x, aes(x = type, y = freq, label = n, fill = label)) +
      geom_col(width = 1)  +
      labs(x = "Label", y = "Relative Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.title = element_blank()) +
      geom_text(size = 3, position = position_stack(vjust = 0.5))
  }
})

output$select_db_wcount <- renderUI({
  cases <- dbcases()
  dblabels <- dblabels() %>%
    dplyr::filter(., opt%in%c("rmstopw"))
  if(nrow(dblabels) == 0) return(NULL)
  tagList(
    dblabels %>%
      split(., seq(nrow(.))) %>%
      lapply(., function(dbl) {
        track <- ifelse(dbl$type=="model","mw2v",dbl$type)
        selectInput(paste0("select_",dbl$opt), label = dbl$label,
                    choices = sort(unique(cases[[track]][[dbl$opt]])),
                    selected = dbl$selected)
      })
  )
})
output$gg_db_wcount <- renderPlotly({
  x <- bow() %>% rowSums(.) %>% data.frame(n = .) %>%
    tibble::rownames_to_column(., var = "text")
  gg <- ggplot(x, aes(x=n)) + geom_histogram() +
    labs(x = "# words in text")
})