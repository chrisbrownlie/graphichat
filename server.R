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
                  div(
                    style = "margin:auto;",
                    uiOutput("report_title"),
                    uiOutput("time_span")
                  )
                ),
                
                br(),
                br(),
                
                fluidRow(
                    div(class = "card-holder",
                        div(
                          uiOutput("card_total_messages")
                        )
                  ),
                  div(class = "card-holder",
                      div(
                        uiOutput("card_average_messages")
                      )
                  ),
                  div(class = "card-holder",
                      div(
                        uiOutput("card_active_days")
                      )
                  )
                ),
                
                hr(),
                
                fluidRow(
                  div(class = "table-container",
                    div(class = "stat-table-title",
                        "Most Messages Sent"),
                    div(class = "stat-table",
                        dataTableOutput("total_messages")
                        )
                    ),
                  div(class = "table-container",
                    div(class = "stat-table-title",
                        "Most Images Sent"),
                    div(class = "stat-table",
                           dataTableOutput("total_images")
                         )
                  ),
                  div(class = "table-container",
                    div(class = "stat-table-title",
                        "Most GIFs Sent"),
                    div(class = "stat-table",
                           dataTableOutput("total_gifs")
                         )
                  ),
                  div(class = "table-container",
                    div(class = "stat-table-title",
                        "Most Emojis Sent"),
                    div(class = "stat-table",
                           dataTableOutput("total_emojis")
                        )
                    )
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
      clean_data <<- clean_dataframe(file_info$path)
      poss_chatname <<- possible_chat_name(file_info$path)
      unique_senders <<- isolate(clean_data) %>%
        select(sender) %>%
        unique() %>%
        mutate(align_number = row_number()%%3)
      print(unique_senders)
      tagList(
        p(style = "font: 14px 'Verdana';color: #19ff34",
          align = "center",
          "Success!"),
        p(align = "center", 
          "Optionally choose alternative names and colours for the identified participants."),
        lapply(seq_along(unique_senders$sender),
               function(i){
                 column(width = 4,
                        height = "50px",
                        offset = 0,
                        div(
                          div(class = "alias-input",
                   textInput(inputId = paste0("alias_", i),
                             label = paste0(unique_senders$sender[i], ": "),
                             value = paste0(unique_senders$sender[i]),
                             width = "200px")
                   ),
                   div(class = "color-input",
                       colourInput(inputId = paste0("pcp_colour_", i),
                                   label = "",
                                   palette = "square",
                                   value = rep(brewer.pal(8, "Dark2"),10)[i],
                                   showColour = "background")
                   )
                 )
                 )
               }),
        br(),
        textInput(inputId = "chatname",
                  label = "Optionally enter a name for the chat/report: ",
                  value = ifelse(length(unique_senders$sender)>2,
                                 coalesce(poss_chatname, "My group chat"),
                                 "My chat"))
      )
    } else {
      p(style = "font: 14px 'Verdana';color: #ff1938",
        align = "center",
        "This does not seem to be a valid .txt file. Please check you uploaded the correct file.")
    }})
   
   
   # Prepare data and graphs for report page
   aliased_data <- reactiveValues(df = NA,
                                  stats = NA)
   observeEvent(input$generate, {
     alias_lookup <- data.frame(sender = unique_senders$sender, stringsAsFactors = FALSE)
     for (i in seq_along(alias_lookup$sender)) {
       alias_lookup$alias[i] <- isolate(input[[paste0("alias_", i)]])
       alias_lookup$colour[i] <- isolate(input[[paste0("pcp_colour_", i)]])
     }
     al_df <- alias_names(clean_data, alias_lookup)
     aliased_data$df <- al_df
     aliased_data$stats <- key_stats(clean_df = al_df)
   })
   
   # Reactive objects for report page
   output$report_title <- renderUI({
     h5(class = "report-title",
        input$chatname)
   })
   output$time_span <- renderUI({
     days <- aliased_data$df$date
     start <- min(days)
     end <- max(days)
     h6(class = "time-span",
        paste0(format.Date(start, format = "%B %Y"), " - ", format.Date(end, format = "%B %Y")))
   })
   
   # Headline figures for report
   output$card_total_messages <- renderUI({
     num <- sum(aliased_data$stats$total_messages)
     div(
       style = "width: 350px",
       div(class = "figure-card",
       formatC(num,
             big.mark = ",",
             digits = nchar(num))
       ),
       div(
         style = "text-align:center;",
         p("Total Messages Sent")
       )
     )
   })
   
    output$card_average_messages <- renderUI({
      num <- sum(aliased_data$stats$total_messages)
     days <- unique(aliased_data$df$date)
     diff <- max(days)-min(days)
     avg <- round(num/as.numeric(diff), 2)
     div(
       style = "width: 350px",
       div(class = "figure-card",
       formatC(avg,
             big.mark = ",",
             digits = nchar(avg)),
       ),
       div(
         style = "text-align:center;",
         p("Average messages per day")
       )
     )
   })
    
    output$card_active_days <- renderUI({
     days <- unique(aliased_data$df$date)
     diff <- max(days)-min(days)
     active_pct <- length(days)/as.numeric(diff)
     div(
       style = "width: 350px",
       div(class = "figure-card",
           paste0(round(active_pct*100,0), "%")
       ),
       div(
         style = "text-align:center;",
         p("Active Days")
       )
     )
   })
   
   # Various small tables for 
   output$total_messages <- renderDataTable({
     stat_table(aliased_data$stats,
                total_messages,
                "Messages Sent")
   })
   output$total_images <- renderDataTable({
     stat_table(aliased_data$stats,
                total_images,
                "Images Sent")
   })
   output$total_gifs <- renderDataTable({
     stat_table(aliased_data$stats,
                total_gifs,
                "GIFs Sent")
   })
   output$total_emojis <- renderDataTable({
     stat_table(aliased_data$stats,
                total_emojis,
                "Emojis Sent")
   })
   
}