dashboardPage(
  dashboardHeader(
    title = "word2vec study"
  ),
  dashboardSidebar(
    selectInput(inputId = "db", label = "Dataset", multiple = F,
                choices = list("Amazon"="amzn",
                               "Processos"="proc",
                               "Reuters"="nltk.reuters",
                               "Petições"="ptin",
                               "IMDB"="imdb"),
                selected = "imdb"),
    sidebarMenu(
      id = "tab",
      menuItem("db info", tabName = "tab_db", selected = T),
      menuItem("w2v embeddings", tabName = "tab_w2v"),
      menuItem("prediction models", tabName = "tab_mw2v"),
      menuItem("feature importance", tabName = "tab_mxgb")
    )
  ),
  dashboardBody(uiOutput("container"))
)
