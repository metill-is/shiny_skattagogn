#### UI ####

aldurshopar_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId = NS(id, "name"),
                label = "Breyta til að skoða",
                choices = aldurshopar_input_names,
                selected = c("Eignir alls"),
                multiple = FALSE,
                selectize = FALSE
            ),
            selectInput(
                inputId = NS(id, "verdlag"),
                label = "Verðlag",
                choices = c("Verðlag hvers árs", "Núvirt"),
                selected = "Núvirt"
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
                br(),
                tabPanel("Skipting", plotlyOutput(NS(id, "dreifing_plot"), height = 700, width = "100%") |> withSpinner()),
                tabPanel("Þróun", plotlyOutput(NS(id, "magn_plot"), height = 700, width = "100%") |> withSpinner())
            )
        )
    )
    
    
}


#### SERVER ####

aldurshopar_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        aldurshopar_df <- reactive({
            
            
            plot_dat <- aldurshopar |> 
                filter(name %in% c(input$name, "Fjöldi í hóp")) |> 
                pivot_wider() |> 
                rename(fjoldi = "Fjöldi í hóp") |> 
                pivot_longer(c(-ar, -skyribreyta, -fjoldi, -cpi)) |> 
                group_by(ar, name) |> 
                mutate(p = value / sum(value),
                       p_fjoldi = fjoldi / sum(fjoldi)) |> 
                ungroup() |> 
                mutate(value_adj = value / cpi)
            
            
            
            plot_dat$plot_value <- plot_dat[[aldurshopar_verdlag(input)]]
            
            plot_dat |> 
                mutate(per_pers = plot_value / fjoldi * 1e6,
                       text = str_c("Ár: ", ar, "\n",
                                    "Aldur: ", skyribreyta, "\n",
                                    "Fjöldi í hóp: ", fjoldi, "\n",
                                    "Hlutfall fólks: ", percent(p_fjoldi, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                    "Hlutfall (breytu): ", percent(p, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
                                    "Samtals: ", number(plot_value, suffix = " mkr", big.mark = ".", decimal.mark = ",")),
                       text = case_when(
                           str_detect(name, "[Tt]ekjur") ~ str_c(text, "\n", 
                                                                 "Á mann (á mánuði): ", number(per_pers / 12, suffix = " kr", accuracy = 1, 
                                                                                               big.mark = ".", decimal.mark = ",")),
                           TRUE ~ str_c(text, "\n", 
                                        "Á mann: ", number(per_pers, suffix = " kr", accuracy = 1, 
                                                           big.mark = ".", decimal.mark = ","))
                       )) 
            
            
        })
        
        dreifing_plot <- reactive({
            aldurshopar_df() |> 
                aldurshopar_dreifing_fig()
        })
        
        magn_plot <- reactive({
            aldurshopar_df() |> 
                aldurshopar_magn_fig(input = input)
        })
        
        output$dreifing_plot <- renderPlotly({
            dreifing_plot() |> 
                aldurshopar_make_plotly()
        }) |> 
            bindCache(
                input$name,
                input$verdlag
            ) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )
        
        output$magn_plot <- renderPlotly({
            magn_plot() |> 
                aldurshopar_make_plotly()
        }) |> 
            bindCache(
                input$name,
                input$verdlag
            ) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )
        
    })
}
