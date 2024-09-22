# app/view/deciles.R

box::use(
  ggiraph[girafeOutput, renderGirafe],
  shiny[moduleServer, NS, plotOutput, reactive, renderPlot, selectInput, tagList],
)

box::use(
  app / logic / deciles[create_decile_plot, load_data, prep_data]
)

d_deciles <- load_data()

#' @export
ui <- function(id) {
  ns <- NS(id)

  box::use(
    bslib[page_sidebar, sidebar, card]
  )

  page_sidebar(
    sidebar = sidebar(
      selectInput(
        ns("plot_var"),
        "Breyta",
        choices = unique(d_deciles$name)
      ),
      selectInput(
        ns("order_var"),
        "Raða eftir",
        choices = unique(d_deciles$order_var)
      )
    ),
    girafeOutput(ns("decile_plot"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    plot_data <- reactive({
      prep_data(d_deciles, input$order_var, input$plot_var)
    })

    output$decile_plot <- renderGirafe({
      create_decile_plot(plot_data(), input$plot_var, input$order_var)
    })
  })
}
