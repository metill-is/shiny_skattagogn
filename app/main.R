box::use(
  shiny[moduleServer, NS, tagList],
  ggplot2[theme_set],
  bslib[bs_theme, bs_add_rules, page_fillable],
  sass[sass_file]
)


box::use(
  view / deciles,
  logic / plot_theme
)

theme_set(plot_theme$theme_metill())

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    theme = bs_theme(bootswatch = "flatly") |> 
      bs_add_rules(sass_file("app/styles/main.scss")),
    deciles$ui(ns("deciles"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    deciles$server("deciles")
  })
}
