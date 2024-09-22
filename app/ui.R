box::use(
  shiny[navbarPage, tabPanel],
  bslib[bs_theme]
)

box::use(
  app / main
)

ui <- navbarPage(
  title = "Skattagögn Hagstofunnar",
  theme = bs_theme(version = 5),
  tabPanel(
    title = "Tíundir",
    main$ui("main")
  )
)
