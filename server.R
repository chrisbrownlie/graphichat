# Define server logic
server <- function(input, output, session) {
  
  output$landing_ui <- renderUI({
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
          div(
            align = "center",
            actionButton(inputId = "generate",
                         label = "Generate Report",
                         icon = icon("comments"),
                         style = 'padding:15px; font-size:16px;')
          ),
          br()
        ),
        
        fluidRow(
          br(),
          column(8, offset = 2,
                 p(class = "info",
                   "To get a copy of your Whatsapp chat data, go on the app and open the info page of the chat you wish to analyse. Select 'Export chat' - 'Without Media' and then save the resulting text file. Upload the file above and click 'Generate Report' to produce a series statistics and visualisations.")
                 )
        ),
        fluidRow(
          column(8, offset = 2,
                 p(class = "info",
                   "Note that no data is stored so if you close down your report you will have to upload the text file again."))
        )
        
      )
  })
}