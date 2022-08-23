#### UI ####

skuldahlutfall_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$style(type="text/css", "body {padding-top: 80px;}"),
            selectInput(
                inputId = NS(id, "hlutf_tegund"),
                label = "Skoða skuldir sem hlutfall af",
                choices = c("Eignum" = "Eignir", 
                            "Ráðstöfunartekjum" = "Ráðstöfunartekjur"),
                selected = "Aldri",
                multiple = FALSE,
                selectize = FALSE
            ),
            selectInput(
                inputId = NS(id, "group_var"),
                label = "Lita eftir",
                choices = c("Aldri", "Skuldahlutfalli"),
                selected = "Aldri",
                multiple = FALSE,
                selectize = FALSE
            ),
            tabsetPanel(
                id = NS(id, "params"),
                type = "hidden",
                tabPanel(
                    "Aldri",
                    selectInput(
                        inputId = NS(id, "filter_skuldahlutfall"), 
                        label = "Skuldahlutfall",
                        choices = skuldahlutfall_hlutf_levels,
                        selected = ">500%",
                        multiple = FALSE,
                        selectize = FALSE
                    )
                ),
                tabPanel(
                    "Skuldahlutfalli",
                    selectInput(
                        inputId = NS(id, "filter_aldur"), 
                        label = "Aldurshópur",
                        choices = skuldahlutfall_age_levels,
                        selected = "Alls",
                        multiple = FALSE,
                        selectize = FALSE
                    )
                )
            ),
            
            div(
                actionButton(
                    inputId = NS(id, "goButton"),
                    label = "Sækja gögn",
                    width = "120px"
                ),
                class = "center", align = "middle"
            ),
            HTML(sidebar_info)
        ),
        
        mainPanel(
            tabsetPanel(
                id = NS(id, "skuldahlutfall_results"),
                br(),
                tabPanel("Dreifing", plotlyOutput(NS(id, "dreifing_plot"), height = 700, width = "100%") |> withSpinner()),
                tabPanel("Hlutfall", plotlyOutput(NS(id, "hlutf_plot"), height = 700, width = "100%") |> withSpinner()),
                tabPanel("Fjöldi", plotlyOutput(NS(id, "magn_plot"), height = 700, width = "100%") |> withSpinner())
            )
        )
    )
    
    
}


#### SERVER ####

skuldahlutfall_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        observeEvent(input$group_var, {
            updateTabsetPanel(inputId = "params", selected = input$group_var)
            
            if (input$group_var == "Skuldahlutfalli") {
                updateTabsetPanel(inputId = "skuldahlutfall_results", selected = "Dreifing")
                hideTab(inputId = "skuldahlutfall_results", target = "Hlutfall")
            } else {
                showTab(inputId = "skuldahlutfall_results", target = "Hlutfall")
            }
        })
        
        observeEvent(input$hlutf_tegund, {
            if (input$hlutf_tegund == "Eignir") {
                updateSelectInput(inputId = "filter_skuldahlutfall", choices = skuldahlutfall_eignir_levels, selected = "> 150%")
            } else {
                updateSelectInput(inputId = "filter_skuldahlutfall", choices = skuldahlutfall_hlutf_levels, selected = ">500%")
            }
        })
        
        
        skuldahlutfall_df <- reactive({
            
            make_skuldahlutfall_data(input)
            
            
        }) 
        
        dreifing_plot <- reactive({
            
            skuldahlutfall_df() |>
                skuldahlutfall_dreifing_fig(input = input)
        })
        
        magn_plot <- reactive({
            
            skuldahlutfall_df() |>
                skuldahlutfall_magn_fig(input = input)
        })
        
        
        hlutf_plot <- reactive({
            skuldahlutfall_df() |>
                skuldahlutfall_hlutf_fig(input = input)
        })
        
        output$dreifing_plot <- renderPlotly({
            dreifing_plot() |>
                skuldahlutfall_make_plotly(input = input)
        }) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        output$magn_plot <- renderPlotly({
            magn_plot() |>
                skuldahlutfall_make_plotly(input = input)
        }) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        
        output$hlutf_plot <- renderPlotly({
            hlutf_plot() |> 
                skuldahlutfall_make_plotly(input = input)
        }) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
    })
}
