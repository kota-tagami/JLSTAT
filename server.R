server <- function(input, output) {
  output$population_total <- renderPlotly({
    get_plot_population_total()
  })
}
