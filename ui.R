ui <- navbarPage("Skattagögn Hagstofunnar",
                 theme = light,
                 position = "fixed-top",
  
                 #### TÍUNDAMÖRK ####
                 tabPanel(
                     title = "Tíundir",
                     tiundamork_ui("tiundamork")
                 ),
                 
                 #### ALDURSHÓPAR ####
                 tabPanel(
                     title = "Aldurshópar",
                     aldurshopar_ui("aldurshopar")
                 ),
                 
                 #### SKULTAHLUTFALL ####
                 tabPanel(
                     title = "Skuldahlutfall",
                     skuldahlutfall_ui("skuldahlutfall")
                 )
                 
                 
                 
)
