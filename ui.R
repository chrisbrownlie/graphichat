# Define UI - mostly defined reactively
tagList(
  
  useShinyjs(),
  
  fluidPage(
    theme = "custom.css",
    
    # Main reactive UI defined in server
    mainPanel(
      width = 12,
      uiOutput("page_ui")
      ),
    
    # Footer for referencing
    tags$footer(
      class = "footer",
      br(),
      p("Created by Chris Brownlie, ",
      a("repo available on Github", href = "https://www.github.com/chrisbrownlie")),
      align = "center"
      )
    )
  
  )