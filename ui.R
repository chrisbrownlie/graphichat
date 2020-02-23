# Define UI
fluidPage(theme = "custom.css",
            
            mainPanel(width = 12,
                  uiOutput("page_ui")),
          
             tags$footer(
               class = "footer",
               tags$a("Chris Brownlie,", 
                      href = "https://medium.com/@chris.brownlie"), 
               " 2020",
               align = "center")
          )


