#' @export
decile_ui <- function(id) {
  tagList(
    selectInput(
      inputId = NS(id, "decile_var"),
      label = "Breyta til að skoða",
      choices = decile_vars,
      selected = "Eignir"
    )
  )
}


#' @export
decile_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$decile_plot <- renderPlot({
      create_decile_plot(input$decile_var)
    })
  })
}
