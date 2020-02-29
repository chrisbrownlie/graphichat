# Define server logic
server <- function(input, output, session) {
  
  # Reactive UI objects -----
  # Define main UI with conditional panels depending on whether report has been generated
  output$page_ui <- renderUI({
    mainPanel(theme = "custom.css",
              width = 12,
              height = "1000px",
              
              # Conditional UI panel for landing page, only showing before 'Generate report' has been clicked
              conditionalPanel(
                condition = "input.generate == 0",
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
                                label = "Upload your Whatsapp chat file here:")
                      )
                  ),
                
                fluidRow(
                  column(12, div(
                    uiOutput("valid_file_container")))
                  ),
                
                fluidRow(
                  div(align = "center",
                      # Initialise 'Generate report' button as disabled as protection against attempting to run before file input
                      disabled(
                        actionButton(inputId = "generate",
                                   label = "Generate Report",
                                   icon = icon("comments"),
                                   style = 'padding:15px; font-size:16px;')
                      )
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
              
              
              # Conditional panel to show report once 'Generate report' has been clicked
              conditionalPanel(
                condition = "input.generate == 1",
                
                # Report title
                fluidRow(
                    div(
                      class = "report-title",
                    uiOutput("report_title")
                    )
                ),
                
                # Report timespan
                fluidRow(
                  div(
                      style = "position:relative;top:0px;vertical-align:top;margin-top:0;",
                      uiOutput("time_span")
                    )
                ),
                
                # Section 1: start with key high-level statistics on dashboard-style 'cards'
                hr(),
                fluidRow(
                  h4(class = 'report-heading',
                     "Key statistics")
                ),
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
                
                # Section 2: leaderboards of participants for various categories
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
                
                hr(),
                
                # Section 3: Graph to show how volume of messages varies over time, with additional options
                fluidRow(
                  h4(class = 'report-heading',
                     "Timing of messages")
                ),
                fluidRow(
                  column(
                    9,
                    plotOutput("messages_over_time")
                  ),
                  column(
                    3,
                    div(
                    p(class = "info",
                      "Options for graph:"),
                    checkboxInput("motplot_split",
                                  label = "Split by sender?",
                                  value = FALSE),
                    checkboxInput("motplot_smooth",
                                  "Show smoothed average?",
                                  value = FALSE),
                    dateRangeInput("motplot_date_range",
                                     label = "Select a date range to zoom on:",
                                     format = "dd-mm-yyyy",
                                     weekstart = 1),
                    checkboxInput("motplot_date_range_flag",
                                  "Apply zoom",
                                  value = FALSE),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    p(style = "font: 10px 'Verdana';font-style:italic;bottom:0px", 
                      "Please note that some combinations of these options may not work if there are not enough messages in the dataset.")
                    )
                  )
                ),
                
                hr(),
                # Section 4: Graph to show volume of messages varies over an average day, with additional options
                fluidRow(
                  column(
                    9,
                    plotOutput("messages_over_day")
                  ),
                  column(
                    3,
                    div(
                    p(class = "info",
                      "Options for graph:"),
                    checkboxInput("modplot_split",
                                  label = "Split by sender?",
                                  value = FALSE),
                    checkboxInput("modplot_smooth",
                                  label = "Show smoothed average?",
                                  value = FALSE),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    p(style = "font: 10px 'Verdana';font-style:italic;bottom:0px", 
                      "Please note that some combinations of these options may not work if there are not enough messages in the dataset.")
                    )
                  )
                ),
                
                hr(),
                # Section 5: Tables to show the most used words for each participant, with additional options
                fluidRow(
                  h4(class = "report-heading",
                     "Most common words")
                ),
                fluidRow(
                  column(
                    9,
                    uiOutput("top_words_by_sender")
                  ),
                  fluidRow(
                    column(
                      3,
                      checkboxInput("remove_stops",
                                    label = "Remove stopwords ('a', 'the', 'and' etc.)?",
                                    value = TRUE),
                      checkboxInput("remove_numbers",
                                    label = "Remove numbers?",
                                    value = TRUE),
                      textInput("custom_remove",
                                label = "Enter words separated by a space to remove them from the analysis:"),
                      textInput("custom_search",
                                label = "Enter a word here to search for it:"),
                      numericInput("num_words",
                                   label = "Number of words to show per sender:",
                                   value = 2,
                                   min = 1,
                                   max = 20)
                    )
                  )
                ),
                
                hr(),
                # Section 6: Tables to show most commonly used emojis by participant, with additional options
                fluidRow(
                  h4(class = "report-heading",
                     "Most common emojis")
                ),
                fluidRow(
                  column(
                    9,
                    uiOutput("top_emojis_by_sender")
                  ),
                  fluidRow(
                    column(
                      3,
                      numericInput("num_emojis",
                                   label = "Number of emojis to show per sender:",
                                   value = 2,
                                   min = 1,
                                   max = 20)
                    )
                  )
                ),
                
                hr(),
                
                # Button to allow starting again (refreshing the session)
                fluidRow(
                  column(12,
                         div(
                           style = "margin:auto;width:50px;",
                           actionButton(inputId = "refresh",
                                      label = "Start again",
                                      icon = icon("return")))
                         )
                  )
                )
              )
    
    })

  # Second-level reactive UI to allow for showing a loading icon after a file has been uploaded,
  # - necessary as some large chats can take several seconds to be read in 
  output$valid_file_container <- renderUI({
    if (length(input$indata)>0) {
      withSpinner(type = 4,
                  color = "#91e368",
                  proxy.height = "160px",
                  uiOutput("valid_file"))
    } else {
      div(style="height:1px;")
    }
  })
  
  # Third-level reactive UI which is rendered within valid_file_container, checks the file
  # is either a text file, in which case it strips out the names of participants and allows the
  # user to define alias names and colours for each one. Alternatively if the input is a
  # zip file (the standard output of a Whatsapp chat export), then the .txt file is first extracted
  # and then the same functionality is provided
  output$valid_file <- renderUI({
    if (is.na(file_info$type)) {
       
      } else if(file_info$type == "application/zip") {
         print("unzipping")
         unzip(file_info$path, files = "_chat.txt", exdir = "temp")
         file_info$path <- "temp/_chat.txt"
         file_info$type <- "text/plain"
         print("unzipped")
       }

    if (is.na(file_info$type)) {
      
    } else if (file_info$type == "text/plain") {
      clean_data <<- clean_dataframe(file_info$path)
      poss_chatname <<- possible_chat_name(file_info$path)
      unique_senders <<- isolate(clean_data) %>%
        select(sender) %>%
        unique() %>%
        mutate(align_number = row_number()%%3)
      x <- tagList(
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
                  label = "Optionally enter a name for the chat report: ",
                  value = ifelse(length(unique_senders$sender)>2,
                                 coalesce(poss_chatname, "My group chat"),
                                 "My chat")),
        enable("generate")
      )
      return(x)
    } else {
      p(style = "font: 14px 'Verdana';color: #ff1938",
        align = "center",
        "This does not seem to be a valid Whatsapp chat .txt or .zip file. Please check you uploaded the correct file.")
    }})
  
  # Reactive value holders -----
  # Handling for file uploading on landing page
  file_info <- reactiveValues(type = NA,
                              size = NA,
                              path = NA,
                              sender_names = c(),
                              sender_aliases = c())
  
  # Main store for data once cleaned and aliased
  aliased_data <- reactiveValues(df = NA,
                                 stats = NA)
  
  # Observers -----
  
  # Observer 1
  # TRIGGER: 'Start again' button is clicked
  # RESPONSE: reload the session
  observeEvent(input$refresh, {
    session$reload()
  })
  
  # Observer 2
  # TRIGGER: file is uploaded
  # RESPONSE: reactive values are updated to allow checks for file input validity
  observeEvent(input$indata, {
    file_info$type <- isolate(input$indata$type)
    file_info$size <- isolate(input$indata$size)
    file_info$path <- isolate(input$indata$datapath)
  })
   
  # Observer 3
  # TRIGGER: 'Generate report' button is clicked
  # RESPONSE: alias dataframe is created and then the input chat data is
  # run through the alias_names function. Finally, aggregated stats dataframe
  # is produced by the key_stats function.
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
   
  # Report Section 0: Title and timespan -----
  # Report name that user has optionally specified
  output$report_title <- renderUI({
     h5(class = "report-title",
        input$chatname)
   })
  
  # Time span for the report
  output$time_span <- renderUI({
     days <- aliased_data$df$date
     start <- min(days)
     end <- max(days)
     h6(class = "time-span",
        paste0(format.Date(start, format = "%B %Y"), " - ", format.Date(end, format = "%B %Y")))
   })
  
  # Report Section 1: Headline figures -----
  # Card: Total messages sent
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
   
  # Card: Average messages sent per day
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
             digits = nchar(avg))
       ),
       div(
         style = "text-align:center;",
         p("Average messages per day")
       )
     )
   })
  
  # Card: % of days where at least one message was sent ('active days')  
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
   
  # Report Section 2: Leaderboard tables -----
  # Stat table: most messages sent
  output$total_messages <- renderDataTable({
     stat_table(aliased_data$stats,
                total_messages,
                "Messages Sent")
   })
   
  # Stat table: most images sent
  output$total_images <- renderDataTable({
     stat_table(aliased_data$stats,
                total_images,
                "Images Sent")
   })
   
  # Stat table: most gifs sent
  output$total_gifs <- renderDataTable({
     stat_table(aliased_data$stats,
                total_gifs,
                "GIFs Sent")
   })
   
  # Stat table: most emojis sent
  output$total_emojis <- renderDataTable({
     stat_table(aliased_data$stats,
                total_emojis,
                "Emojis Sent")
   })
   
  # Report Section 3: Volume of messages over time -----
   # Plot: Line graph of messages over time
   # Options: 
   #      - Split graph out by chat participants
   #      - Zoom in to particular date range
   #      - Show smoothed average using geom_smooth
   output$messages_over_time <- renderPlot({
     
     mot <- aliased_data$df %>%
       count(date)
     
     mot_split <- aliased_data$df %>%
       count(date, sender) %>%
       group_by(sender)
     
     plot_colours <- aliased_data$stats$colour
     
     names(plot_colours) <- aliased_data$stats$sender
     if (input$motplot_split == TRUE) {
       base_plot <- ggplot(mot_split, aes(x = date)) +
         geom_line(aes(y = n, color = sender)) +
         scale_color_manual("Sender", values = plot_colours) +
         ggtitle("Messages over time, by sender")
     } else {
       base_plot <- ggplot(mot, aes(x = date)) +
         geom_line(aes(y = n), color = "#83fa6b") +
         ggtitle("Messages over time")
     }
     if (input$motplot_date_range_flag == TRUE) {
       base_plot <- base_plot + 
         lims(x = c(min(input$motplot_date_range), max(input$motplot_date_range)))
     }
     if (input$motplot_smooth == TRUE & input$motplot_split == FALSE) {
       base_plot <- base_plot + geom_smooth(aes(y = n), se = FALSE, color = "#2176ff")
     } else if (input$motplot_smooth == TRUE & input$motplot_split == TRUE) {
       base_plot <- base_plot + geom_smooth(aes(y = n, color = sender), se = FALSE)
     }
     base_plot +
       labs(x = "Date",
            y = "Number of messages sent") +
       theme_hc()
   })

  # Report Section 4: Volume of messages over average day -----
   # Plot: Line graph of messages over a 24h day
   # Options:
   #      - Split graph out by chat participants
   #      - Show smoothed average using geom_smooth
   output$messages_over_day <- renderPlot({
     
     days <- as.numeric(max(aliased_data$df$date) - min(aliased_data$df$date))
     
     mod <- aliased_data$df %>%
       mutate(minute = as.numeric(substr(time, 1, 2))*60+as.numeric(substr(time, 4, 5)),
              minute = ceiling(minute/5)) %>%
       count(minute) %>%
       mutate(avg_msg_pm = n/days)
     
     mod_split <- aliased_data$df %>%
       mutate(minute = as.numeric(substr(time, 1, 2))*60+as.numeric(substr(time, 4, 5)),
              minute = ceiling(minute/5)) %>%
       count(sender, minute) %>%
       mutate(avg_msg_pm = n/days)
     
     if (input$modplot_split == TRUE) {
       base_plot <- ggplot(mod_split, aes(x = minute)) +
         geom_line(aes(y = avg_msg_pm, color = sender)) +
         ggtitle("Average message volume over the course of the day, by sender")
       
       if (input$modplot_smooth == TRUE) {
         base_plot <- base_plot +
           geom_smooth(aes(y = avg_msg_pm, color = sender), se = FALSE)
       }
       
     } else {
       base_plot <- ggplot(mod_split, aes(x = minute)) +
         geom_line(aes(y = avg_msg_pm), color = "#83fa6b") +
         ggtitle("Average message volume over the course of the day")
       
       if (input$modplot_smooth == TRUE) {
         base_plot <- base_plot +
           geom_smooth(aes(y = avg_msg_pm), color = "#2176ff", se = FALSE)
       }
       
     }
    base_plot +
      labs(x = "Time of day", y = "Average messages/minute") +
      scale_x_continuous(breaks = c(1:24*12),
                         labels = c(paste0(1:11, "am"), "Midday", paste0(1:11, "pm"), "Midnight")) +
      theme_hc() +
      theme(axis.text.x = element_text(angle = 45))
   })

  # Report Section 5: Most commonly used words by participant -----
   # Stat tables: tables of most commonly used words for each chat
   # participant. These are dynamically created as depends on number 
   # of chat participants
   # Options:
   #      - Remove stopwords
   #      - Remove numbers
   #      - Number of words to show for each participant
   #      - Specify words to remove from analysis
   #      - Specify a word to search for (filter on)
   output$top_words_by_sender <- renderUI({

    tw_data <- aliased_data$df %>%
      filter(!image_flag,
             !video_flag,
             !gif_flag,
             !other_media_flag) %>%
      mutate(text = str_replace_all(text, pattern = "/", replacement = " "),
             text = str_replace_all(tolower(text), pattern = "[[:punct:]]", replacement = "")) %>%
      unnest_tokens(input = text,
                    output = "word",
                    token = "words") %>%
      filter(nchar(word)<20)

    if (input$remove_stops) {
      tw_data <- tw_data %>%
        filter(!word %in% tidytext::stop_words$word)
    }
    
    if (input$remove_numbers) {
      tw_data <- tw_data %>%
        filter(!str_detect(word, pattern = "^[[:digit:]]+$"))
    }

    if (input$custom_remove!="") {
      custom_removers <- str_split(input$custom_remove, pattern = "[[:space:][:punct:]]+") %>%
        unlist()
      tw_data <- tw_data %>%
        filter(!word %in% tolower(custom_removers))
    }
    
    if (input$custom_search!="") {
      tw_data <- tw_data %>%
        filter(str_detect(word, pattern = input$custom_search))
    }
      
    tw_data <- tw_data %>%
      group_by(sender, word) %>%
      summarise(num = n(),
                colour = first(colour)) %>%
      arrange(desc(num)) %>%
      group_by(sender) %>%
      slice(1:input$num_words)
    
    senders <- aliased_data$stats$sender

    lapply(seq_along(senders), function(i){
     output[[paste0("topwords_", i)]] <- renderDataTable({
       tw_data %>%
         filter(sender == senders[i]) %>%
         stat_table(column = num, col_alias = "# of times", type = "words")
       })
     })
    fluidRow(
     lapply(seq_along(senders), function(i) {
       div(style = "float:left;margin-right:30px",
       div(
         dataTableOutput(outputId = paste0("topwords_", i))
       )
       )
     })
    )
   })
   
  # Report Section 6: Most commonly used emojis by participant -----
   # Stat table: tables of most commonly used emojis for each chat
   # participant. These are dynamically created as depends on number 
   # of chat participants
   # Options:
   #      - Number of emojis to show for each participant
   output$top_emojis_by_sender <- renderUI({
     
     emoji_data <- aliased_data$df %>%
       filter(emojis>0) %>%
       mutate(emoji = emo::ji_extract_all(text)) %>%
       tidyr::unnest(cols = c(emoji)) %>%
       group_by(sender, emoji) %>%
       summarise(num = n(),
                 colour = first(colour)) %>%
       arrange(desc(num)) %>%
       group_by(sender) %>%
       slice(1:input$num_emojis)
     senders <- aliased_data$stats$sender
     
     lapply(seq_along(senders), function(i){
     output[[paste0("topemojis_", i)]] <- renderDataTable({
       emoji_data %>%
         filter(sender == senders[i]) %>%
         stat_table(column = num, col_alias = "# of times", type = "emojis")
       })
     })
    fluidRow(
     lapply(seq_along(senders), function(i) {
       div(style = "float:left;margin-right:30px",
       div(
         dataTableOutput(outputId = paste0("topemojis_", i))
       )
       )
     })
    )
       
       
     
     
   })
   
}