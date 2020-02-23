# Define server logic
server <- function(input, output, session) {
  
  # Reactive value for determining which page to be showing
  # - default to landing page
  output$page_status <- reactive({
    tryCatch({if (input$generate==0) {
      "landing"
      } else if (input$generate == 1) {
      "report"
      }
    }, error = function(e) {"landing"})
      
  })
  # Ensure page_status is always updated
  outputOptions(output,
                "page_status",
                suspendWhenHidden = FALSE)
  
  # Define main UI with conditional panels depending on page_status
  output$page_ui <- renderUI({
    mainPanel(theme = "custom.css",
              width = 12,
              height = "1000px",
              
              # Conditional panel for landing page
              conditionalPanel(
                condition = "output.page_status == 'landing'",
                fluidRow(
                  br(),
                  br(),
                  br(),
                  br(),
                  h1("Graphichat", 
                     class = "maintitle", 
                     align = "center"),
                  h3("The Whatsapp Chat Visualiser",
                     align = "center")
                  ),
        
                fluidRow(
                  column(12, div(style = "height:50px"))
                  ),
        
                fluidRow(
                  br(),
                  br(),
                  div(align = "center",
                      class = "info",
                      fileInput(inputId = "indata",
                                label = "Upload your Whatsapp .txt file here:")
                      )
                  ),
                
                fluidRow(
                  column(12, div(
                         uiOutput("valid_file")))
                  ),
                
                fluidRow(
                  div(align = "center",
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
                ),
              
              
              # Page for loading screen
              conditionalPanel(
                condition = "output.page_status == 'report'",
                
                fluidRow(
                  column(12, p("Report page"))
                ),
                
                fluidRow(
                  column(4, offset = 4,
                         actionButton(inputId = "refresh",
                                      label = "Start again",
                                      icon = icon("return")))
                )
              )
              )
    
    })
  
  # Refresh app if return button pressed on report page
  observeEvent(input$refresh, {
    session$reload()
  })
  
  # Handling for file uploading on landing page
  file_info <- reactiveValues(type = NA,
                              size = NA,
                              path = NA,
                              sender_names = c(),
                              sender_aliases = c())
  
  # Update values on upload
  observeEvent(input$indata, {
    print(input$indata$type)
    file_info$type <- isolate(input$indata$type)
    file_info$size <- isolate(input$indata$size)
    file_info$path <- isolate(input$indata$datapath)
  })
  
   output$valid_file <- renderUI({
    if (is.na(file_info$type)) {
      
    } else if (file_info$type == "text/plain") {
      tagList(
        p(style = "font: 14px 'Verdana';color: #19ff34",
          align = "center",
          "Success!"),
        p(align = "center", 
          "Optionally choose alternative names for the identified senders.")
      )
    } else {
      p(style = "font: 14px 'Verdana';color: #ff1938",
        align = "center",
        "This does not seem to be a valid .txt file. Please check you uploaded the correct file.")
    }})
}