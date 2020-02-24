library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)
library(lubridate)
library(colourpicker)
library(RColorBrewer)
library(DT)

# Get possible chat name
possible_chat_name <- function(text_file_input_path) {
  # Read input file
  raw <- readLines(text_file_input_path)
  
  possible_name <- raw[1] %>%
    str_extract(pattern = "(?<=[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}, [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}\\])[[:print:]]+(?=: )") %>%
    trimws()
  
  return(possible_name)
}

# Define functions for analysing Whatsapp data
clean_dataframe <- function(text_file_input_path) {
  
  # Read input file
  raw <- readLines(text_file_input_path)
  
  cat("\n..Beginning analysis...\n")
  cat("Stripping out non-messages and fixing multi-line messages...\n")
  if (str_detect(raw[1], pattern = "Messages to this (chat|group)( and calls)? are now secured with end-to-end encryption.")) {
    raw <- raw[-1]
  }
  raw_vec <- raw[raw!=""]
  for (i in rev(seq_along(raw_vec)[-1])) {
    if (str_detect(raw_vec[i], pattern = "^[^\\[]")) {
        raw_vec[i] <- substr(raw_vec[i], 2, nchar(raw_vec[i]))
      }
    if (!str_detect(raw_vec[i], pattern = "^\\[[[:digit:]]{2}")) {
      raw_vec[i-1] <- paste0(raw_vec[i-1], " ", raw_vec[i])
      raw_vec[i] <- NA
    }
  }
  raw_vec2 <- raw_vec[!is.na(raw_vec)]
  
  # Transform into dataframe for easier use
  raw_df <- data.frame(raw_text = raw_vec2,
                       stringsAsFactors = FALSE) %>%
    mutate(date = dmy(substr(raw_text, 2, 11)),
           time = substr(raw_text, 14, 21),
           stripped_text = substr(raw_text, 23, nchar(raw_text)),
           sender = trimws(str_extract(stripped_text, pattern = "[[:print:]]+?(?=: )")),
           text = str_extract(stripped_text, pattern = "(?<=: ).+$"),
           text = ifelse(str_detect(text, pattern = "(?<=^)[[:alnum:][:space:][:punct:]]"),
                         text,
                         substr(text, 2, nchar(text))),
           image_flag = text=="image omitted",
           video_flag = text=="video omitted",
           gif_flag = text=="GIF omitted",
           other_media_flag = text %in% c("sticker omitted", "Contact card omitted"),
           emojis = emo::ji_count(text)) %>%
    filter(!is.na(text)) %>%
    select(date, time, sender, text, image_flag, video_flag, gif_flag, other_media_flag, emojis)
  
  return(raw_df)
}

# Function for aliasing names
alias_names <- function(dataframe, alias_df){
  new_dataframe <- dataframe %>%
    left_join(alias_df, by = "sender") %>%
    select(date, 
           time, 
           "sender" = alias, 
           text, 
           image_flag, 
           video_flag, 
           gif_flag, 
           other_media_flag, 
           emojis, 
           colour)
  return(new_dataframe)
}


key_stats <- function(clean_df) {
  
  sender_totals <- clean_df %>%
    group_by(sender) %>%
    summarise(colour = first(colour),
              total_messages = n(),
              total_images = sum(image_flag),
              total_videos = sum(video_flag),
              total_gifs = sum(gif_flag),
              other_media = sum(other_media_flag),
              average_message_length = round(mean(nchar(text)), 2),
              longest_message_length = max(nchar(text)),
              total_emojis = sum(emojis),
              average_emojis = total_emojis/total_messages)
  
  return(sender_totals)
}



# Produce stats table
stat_table <- function(stat_data, column, col_alias) {
  
  column <- enquo(column)
  dat <- stat_data %>%
       arrange(desc(!!column)) %>%
       select("Sender" = sender,
              !!column,
              colour)
  names(dat) <- c("Sender", col_alias, "colour")
     dt <- dat %>%
       datatable(rownames = FALSE,
                 options = list(dom = 't',
                                ordering = FALSE,
                                columnDefs = list(list(visible = FALSE, targets = 2)))) %>%
       formatStyle("colour",
                   target = "row",
                   color = styleEqual(dat$colour, dat$colour))
     return(dt)
}