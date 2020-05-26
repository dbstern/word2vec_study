tagList(
  tabItem(
    tabName = "db",
    bsCollapse(
      id = "db_collapse",
      bsCollapsePanel(
        "Relative Frequency of Labels",
        uiOutput("select_db_freq"),
        plotlyOutput("gg_db_freq")
      ),
      bsCollapsePanel(
        "Word Count",
        uiOutput("select_db_wcount"),
        plotlyOutput("gg_db_wcount")
      )
    )
  )
)
