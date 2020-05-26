tagList(
  tabItem(
    tabName = "mw2v",
    wellPanel(
      fluidRow(
        uiOutput("select_mw2v")
      )
    ),
    bsCollapsePanel(
      "Table",
      fluidRow(
        column(width = 2, uiOutput("select_mw2v_tb")),
        column(width = 10, DTOutput("tb_mw2v", height = "300%", width = "100%"))
      )
    ),
    bsCollapsePanel(
      "Plot: Continuous Measures",
      fluidRow(
        column(width = 2, uiOutput("select_mw2v_contm")),
        column(width = 10, plotlyOutput("gg_mw2v_contm", height = "300%", width = "100%"))
      )
    ),
    bsCollapsePanel(
      "Plot: Discrete Measures",
      fluidRow(
        column(width = 2, uiOutput("select_mw2v_discm")),
        column(width = 10, plotlyOutput("gg_mw2v_discm", height = "200%", width = "100%"))
      )
    )
  )
)