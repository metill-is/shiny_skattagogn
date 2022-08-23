#### UI ####

tiundamork_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$style(type="text/css", "body {padding-top: 80px;}"),
            selectInput(
                inputId = NS(id, "tiundarbreyta"),
                label = "Breyta til að skipta í tíundamörk",
                choices = tiundamork_tiundabreytur,
                selected = c("Eignir"),
                multiple = FALSE,
                selectize = FALSE
            ),
            selectInput(
                inputId = NS(id, "name"),
                label = "Breyta til að skoða",
                choices = tiundamork_input_names,
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
            br(" "),
            p(tiundir_text),
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

tiundamork_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        tiundamork_df <- reactive({
            
            
            plot_dat <- tiundamork |> 
                filter(tiundarbreyta == input$tiundarbreyta,
                       name %in% c(input$name, "Fjöldi í hóp")) |> 
                mutate(tiundarhluti = as_factor(tiundarhluti)) |> 
                pivot_wider() |> 
                rename(fjoldi = "Fjöldi í hóp") |> 
                pivot_longer(c(-ar, -tiundarhluti, -fjoldi, -cpi, -tiundarbreyta)) |> 
                group_by(ar, name) |> 
                mutate(p = value / sum(value)) |> 
                ungroup() |> 
                mutate(value_adj = value / cpi)
            
            
            
            plot_dat$plot_value <- plot_dat[[tiundamork_verdlag(input)]]
            
            plot_dat |> 
                mutate(per_pers = plot_value / fjoldi * 1e6,
                       text = str_c("Ár: ", ar, "\n",
                                    "Tíund: ", tiundarhluti, "\n",
                                    "Hlutfall: ", percent(p, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
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
            tiundamork_df() |> 
                tiundamork_dreifing_fig(input = input)
        })
        
        magn_plot <- reactive({
            tiundamork_df() |> 
                tiundamork_magn_fig(input = input)
        })
        
        output$dreifing_plot <- renderPlotly({
            dreifing_plot() |> 
                tiundamork_make_plotly()
        }) |> 
            bindCache(
                input$name,
                input$verdlag,
                input$tiundarbreyta
            ) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )
        
        output$magn_plot <- renderPlotly({
            magn_plot() |> 
                tiundamork_make_plotly()
        }) |> 
            bindCache(
                input$name,
                input$verdlag,
                input$tiundarbreyta
            ) |> 
            bindEvent(
                input$goButton, ignoreNULL = FALSE
            )   
    })
}