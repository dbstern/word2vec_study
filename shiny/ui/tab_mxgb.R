tagList(
  tabItem(
    tabName = "mxgb",
    sidebarPanel(
      width = 3,
      uiOutput("select_mxgb")
    ),
    mainPanel(
      bsCollapse(
        id = "mxgb_collapse",
        bsCollapsePanel(
          "Plot: Grouped Features",
          plotlyOutput("gg_mxgb_imp")
        ),
        bsCollapsePanel(
          "Plot: Grouped Non-Zero Coefficients",
          plotlyOutput("gg_mxgb_imp_nonzero")
        ),
        bsCollapsePanel(
          "Table: Grouped Features",
          DT::dataTableOutput("dt_mxgb_imp_")
        ),
        bsCollapsePanel(
          "Table: Ungrouped Features",
          DT::dataTableOutput("dt_mxgb_imp")
        )
      )
    )
  )
)