output$select_mw2v <- renderUI({
  cases <- dbcases()
  tagList(
    column(
      width=1,
      checkboxGroupInput("select_bow", "", choices = c("bow"="+BOW","bow_cl"="+Clusters"), selected = "bow")
    ),
    dblabels() %>%
      dplyr::filter(., !(type %in% c("na","bow"))) %>%
      split(., seq(nrow(.))) %>%
      purrr::discard(., ~nrow(.) == 0) %>%
      lapply(., function(dbl) {
        column(
          width = ifelse(dbl$opt=="stat",8,2),
          selectInput2(dbl,multiple = T,cases=cases)
        )
        
      }),
    column(
      width = 1,
      actionButton(inputId = "mw2v_go",label = "Go!")
    )
  )
})

mw2v_cases <- eventReactive(input$mw2v_go, {
  dbcases_selected()
})

mw2v_models <- reactive({
  validate(need(nrow(mw2v_cases()) > 0,
                "Select models that have been previously adjusted."))
  mw2v_cases() %>%
    split(., seq(nrow(.))) %>%
    lapply(., function(x) {
      if(!file.exists(x$file_model)) return(NULL)
      data <- readRDS(x$file_model)
      
      data$case <- x
      return(data)
    })
})

mw2v_coords <- reactive({
  mw2v_models() %>%
    lapply(., function(fit) {
      if(!"roc" %in% names(fit)) return(NULL)
      x_best <- pROC::coords(fit$roc, x = "best", ret="all", transpose=F) %>%
        dplyr::mutate(., best = T)
      x_all <- pROC::coords(fit$roc, input="threshold",
                            x = seq(from = 0,to = 1,by = .001),
                            ret="all", transpose=F) %>%
        dplyr::mutate(., best = F)
      x <- rbind(x_best, x_all) %>% cbind(fit$case,.) %>%
        dplyr::mutate(., f1 = (2 * precision * recall) / (precision + recall))
      return(x)
    }) %>% do.call(rbind, .)
})
choices_coords <- reactive({
  setdiff(names(mw2v_coords()), names(mw2v_cases()))
})
mw2v_discm_alpha <- .05
mw2v_discm <- reactive({
  alpha <- mw2v_discm_alpha
  nbonf <- input$select_discm_ci_bonf
  if(is.null(nbonf)) nbonf <- 1
  
  mw2v_models() %>%
    lapply(., function(x) {
      if(!"roc" %in% names(x)) return(NULL)
      ci_auc <- c(
        pROC::ci.auc(x[["roc"]], 
                     conf.level=1-(alpha/nbonf),
                     method="delong")
      )
      ci_gini <- 2*(ci_auc-.5)
      names(ci_auc) <- paste0("auc",c("_inf","","_sup"))
      names(ci_gini) <- paste0("gini",c("_inf","","_sup"))
      x$case %>% cbind(.,t(ci_auc)) %>% cbind(.,t(ci_gini))
    }) %>% do.call(rbind, .) %>%
    dplyr::mutate(., dred_w2v = ifelse(is.na(dred_w2v),"-",dred_w2v))
})
choices_discm <- reactive({
  setdiff(names(mw2v_discm()), names(mw2v_cases()))
})

output$select_mw2v_tb <- renderUI({
  tagList(
    verticalLayout(
      checkboxInput("select_tb_best", "Best", value = T, width = NULL),
      conditionalPanel(
        condition = "input.select_tb_best == false",
        numericInput("select_tb_cut_value", label = "Cutoff", value = .5)
      ),
      selectInput("select_tb_coords", label="Measures",
                  choices=c("model","stat",choices_coords(),choices_discm()),
                  multiple = T,
                  selected = c("model","stat","f1","1-specificity","auc")),
      downloadButton("tb_tex","Download .tex file")
    )
  )
})
tb_mw2v_data <- reactive({
  x <- mw2v_coords()
  if(req(input$select_tb_best)) {
    x <- dplyr::filter(x, best)
  } else {
    x <- dplyr::filter(
      x, near(threshold,as.numeric(input$select_tb_cut_value), tol=.0005))
  }
  
  x <- merge(x, mw2v_discm()) %>%
    dplyr::mutate_if(., is.numeric, round, 3) %>%
    dplyr::select(., c(input$select_tb_coords)) %>%
    unique(.)
  return(x)
})
output$tb_mw2v <- renderDT({
  datatable(
    tb_mw2v_data(),
    rownames = FALSE,
    options = list(
      lengthChange = FALSE,
      dom = 'tip'
    )
  ) #%>%
  # dplyr::mutate_if((is.numeric(.) & !is.integer(.)),formatPercentage,2)
})
output$tb_tex <- downloadHandler(
  filename = function() {
    paste0("tb_",input$db, ".tex")
  },
  content = function(filename) {
    tb_mw2v_data() %>%
      xtable(., type = "latex") %>%
      print(., file = filename)
  }
)

output$select_mw2v_contm <- renderUI({
  choices <- setNames(dblabels()[["opt"]],dblabels()[["label"]]) %>% as.list()
  
  tagList(
    verticalLayout(
      selectInput("select_contm_type", label="", choices=c("Roc"="roc",choices_coords()), selected="auc"),
      selectInput("select_contm_col", label="Color", choices=choices, selected="stat"),
      selectInput("select_contm_line", label="Line", choices=choices, selected="NULL"),
      selectInput("select_contm_fgrid", label="Facet Grid", choices=choices[-1],
                  multiple = T, selected="model")
    )
  )
})
output$gg_mw2v_contm <- renderPlotly({
  vcol <- rlang::sym(req(input$select_contm_col))
  vline <- rlang::sym(input$select_contm_line)
  vgrid <- input$select_contm_fgrid
  ygrid <- if(length(vgrid) > 1) vgrid[-1] else "."
  ygrid <- paste0(ygrid, collapse = "+")
  
  xaxis <- ifelse(input$select_contm_type == "roc","specificity","threshold") %>% rlang::sym(.)
  yaxis <- ifelse(input$select_contm_type == "roc","sensitivity",input$select_contm_type) %>% rlang::sym(.)
  
  x <- mw2v_coords()
  
  if(input$select_contm_col != "NULL")
    x <- dplyr::mutate(x, !!vcol := as.factor(!!vcol))
  if(input$select_contm_line != "NULL")
    x <- dplyr::mutate(x, !!vline := as.factor(!!vline))
  
  gg <- ggplot(x, aes(x=!!xaxis, y=!!yaxis, group=file_model)) +
    geom_line() + 
    aes_string(col=input$select_contm_col, linetype=input$select_contm_line) +
    theme(legend.title = element_blank())
  
  if(input$select_contm_col != "NULL")
    gg <- gg + scale_color_manual(values=gg_color_hue(x[[vcol]]))
  
  if(length(vgrid)>0)
    gg <- gg + facet_grid(paste0(ygrid,"~",vgrid[1]))
  plotly::ggplotly(gg)
})

output$select_mw2v_discm <- renderUI({
  choices <- setNames(dblabels()[["opt"]],dblabels()[["label"]]) %>% as.list()
  
  tagList(
    verticalLayout(
      selectInput("select_discm_type", label="", choices=c("Gini"="gini","AUC"="auc"), selected="auc"),
      checkboxInput("select_discm_ci", "95% CI (DeLong)", value = T, width = NULL),
      conditionalPanel(
        condition = "input.select_discm_ci == true",
        numericInput("select_discm_ci_bonf", label = "#Bonferroni Correction", value = 1, min = 1)
      ),
      selectInput("select_discm_axis", label="Axis", choices=choices[-1], selected="stat"),
      selectInput("select_discm_fill", label="Color", choices=choices, selected="stat"),
      selectInput("select_discm_fgrid", label="Facet Grid", choices=choices[-1],
                  multiple = T, selected="model")
    )
  )
})
output$gg_mw2v_discm <- renderPlotly({
  citype <- paste0(req(input$select_discm_type),c("_inf","","_sup"))
  vaxis <- rlang::sym(input$select_discm_axis)
  vfill <- rlang::sym(input$select_discm_fill)
  vgrid <- input$select_discm_fgrid
  ygrid <- if(length(vgrid) > 1) vgrid[-1] else "."
  ygrid <- paste0(ygrid, collapse = "+")
  
  x <- mw2v_discm()
  
  choices <- setNames(dblabels()[["opt"]],dblabels()[["label"]]) %>% as.list()
  choices <- choices[-1]
  choices[citype[2]] <- citype[2]
  txt <- choices[which(unlist(choices) %in% names(x))]
  txt <- lapply(seq_along(txt), function(idx) {
    paste0(names(txt[idx]), ": ", as.character(x[[txt[[idx]]]]), "<br>")
  }) %>% do.call(paste0,.)
  x$txt <- txt
  
  if(input$select_discm_fill != "NULL")
    x <- dplyr::mutate(x, !!vfill := as.factor(!!vfill))
  
  if(input$select_discm_ci) {
    gg <- ggplot(x, aes(x=!!rlang::sym(citype[2]), y=!!vaxis)) + geom_point() +
      geom_errorbarh(aes(xmin = !!rlang::sym(citype[1]), xmax= !!rlang::sym(citype[3]))) +
      scale_y_discrete(limits = rev((levels(factor(x[[input$select_discm_axis]])))))
    
    if(input$select_discm_fill != "NULL")
      gg <- gg + aes_string(col = input$select_discm_fill) +
        scale_color_manual(values=gg_color_hue(x[[vfill]]))
    
  } else {
    gg <- ggplot(x, aes(x = !!vaxis, y = !!rlang::sym(citype[2]))) +
      scale_x_discrete(limits = rev((levels(factor(x[[input$select_discm_axis]]))))) +
      ylim(0,1) + coord_flip()
    
    if(input$select_discm_fill == "NULL") {
      gg <- gg + geom_bar(stat = "identity", fill = "lightblue")
    } else {
      gg <- gg + geom_bar(stat = "identity", position=position_dodge()) + 
        aes_string(fill = input$select_discm_fill) +
        scale_fill_manual(values=gg_color_hue(x[[vfill]]))
    }
    gg <- gg +  geom_text(aes(label = round(auc,3), y=.5),
                          position = position_dodge(0.9), size=3.5)
  }
  
  if(input$select_discm_fill == input$select_discm_axis)
    gg <- gg + theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank()
    )
  if(length(vgrid) > 0)
    gg <- gg + facet_grid(paste0(ygrid,"~",vgrid[1]), scales = "free")
  
  xtitle <- paste(toupper(as.character(input$select_discm_type)), 
                  if(input$select_discm_ci) paste0("(",(1-mw2v_discm_alpha)*100,"%)"))
  gg <- gg + aes(text = txt) + xlab(xtitle)
  
  gg <- plotly::ggplotly(gg, tooltip = "text") %>%
    layout(legend = list(xanchor = "left", x = 1.035))
  
  idx <- lapply(gg[["x"]][['layout']][['annotations']], function(x) x[["text"]] == xtitle) %>%
    unlist() %>% which()
  if(length(idx) > 0)
    gg[["x"]][['layout']][['annotations']][[idx]][["y"]] <- -.05
  return(gg)
})

