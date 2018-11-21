dashboardPage(
  dashboardHeader(
    title = "word2vec study"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("w2v", tabName = "w2v"),
      menuItem("modelo w2v", tabName = "mw2v")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "w2v",
        sidebarPanel(
          selectInput(
            "select_plot",
            label = "Tipo de Gráfico", 
            choices = list("W2V Prop. 0/1" = "plot_w2v_prop",
                           "W2V Temp" = "plot_w2v_ts"),
            selected = "plot_w2v_ts"
          ),
          selectInput("select_dim", label = "Dim w2v",
                      choices = sort(cases$w2v$w2v_dim),
                      selected = 100),
          checkboxInput("select_pca", "Aplicar PCA", value = T),
          uiOutput("selectors")
        ),
        mainPanel(
          plotlyOutput("plot")
        )
      )
    ),
      tabItem(
        tabName = "mw2v"
      )
  )
)
