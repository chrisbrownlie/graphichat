# UI for landing/upload page
output$page_ui <- renderUI({
    mainPanel(width = 12,
                height = "1000px",
                
        fluidRow(
          br(),
          br(),
          br(),
          br(),
          h1("Graphichat", class = "maintitle", align = "center"),
          h3("The Whatsapp Chat Visualiser", align = "center")
        ),
        
        fluidRow(column(12, div(style = "height:50px"))),
        
        fluidRow(
          br(),
          br(),
          div(align = "center",
            fileInput(inputId = "indata",
                      label = "Upload your Whatsapp .txt file here:")
          )
        ),
        
        fluidRow(
          br(),
          column(8, offset = 2,
                 p(class = "info",
                   "To get a copy of your Whatsapp chat data, go on the app and open the info page of the chat you wish to analyse. Select 'Export chat' - 'Without Media' and then save the resulting text file. Upload the file above to produce a series of interesting statistics and visualisations.")
                 )
        ),
        fluidRow(
          column(8, offset = 2,
                 p(class = "info",
                   "Note that no data is stored so if you close down your report you will have to upload the text file again."))
        )
        
      )
  })