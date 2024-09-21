ui <- shinyUI(
  navbarPage(
    title = "Skattagögn Hagstofunnar",
    theme = bs_global_get(),
    #### TÍUNDAMÖRK ####
    tabPanel(
      title = "Tíundir",
      tiundamork_ui("tiundamork")
    )
  )
)
