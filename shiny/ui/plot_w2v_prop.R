tagList(
  selectInput("indic_geq", label = "Label",
              choices = sort(unique(cases$sample$sample_igeq))),
  conditionalPanel(
    "!(input.select_rdim == 'tsne')",
    uiOutput("select_xy")
  ),
  sliderInput("slider1", label = h3("Slider"),
              min = 0, max = 1, value = c(.40,.60))
)