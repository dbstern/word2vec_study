tagList(
  tabItem(
    tabName = "w2v",
    sidebarPanel( uiOutput("select_w2v") ),
    mainPanel( plotlyOutput("gg_w2v") )
  )
)
