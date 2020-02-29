library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)
library(lubridate)
library(colourpicker)
library(RColorBrewer)
library(DT)
library(ggplot2)
library(ggthemes)
library(tidytext)
library(shinycssloaders)

# Precautionary: make sure file name for unzipping zip file is available
if (file.exists("temp/_chat.txt")){
  file.remove("temp/_chat.txt")
}


# Function definitions ----------
#' Identify group chat name
#' 
#' Get possible chat name from the first message, where relevant
#' 
#' @param text_file_input_path location of the .txt file of a whatsapp group chat
#' 
#' @return the name of the group chat, if relevant
possible_chat_name <- function(text_file_input_path) {
  
  # Read input file
  raw <- readLines(text_file_input_path)
  
  # Extract name of group from first message
  possible_name <- raw[1] %>%
    str_extract(pattern = "(?<=[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}, [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}\\])[[:print:]]+(?=: )") %>%
    trimws()
  
  return(possible_name)
}

#' Clean dataframe
#' 
#' Data pre-processing function for chat .txt file
#' 
#' @param text_file_input_path location of the .txt file of a whatsapp group chat
#' 
#' @return a cleaned dataframe, with one row per message and valid information/flags split into separate columns
clean_dataframe <- function(text_file_input_path) {
  
  # Read input file
  raw <- readLines(text_file_input_path)
  
  # Basic initial steps, notifying of progress
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
  cat("Creating transformed dataframe...\n")
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

#' Alias names
#' 
#' Aliasing names of chat participants
#' 
#' @param dataframe a cleaned chat dataframe, as produced by clean_dataframe()
#' @param alias_df a dataframe of chat participants, their alias (if they have one), 
#' and the colour assigned to them from the landing page UI
#' 
#' @return the original cleaned dataframe, with participant names replaced by aliases
#' and their colours included
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

#' Key participant stats
#' 
#' Produce participant-level statistics for the chat
#' 
#' @param clean_df a cleaned chat dataframe as produced by clean_dataframe() (and optionally, alias_names())
#' 
#' @return a dataframe with one row per sender and aggregated statistics on their participation in the chat
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

#' Formatting of tables
#' 
#' Take in a dataframe and format as a datatable for surfacing in the app
#' 
#' @param stat_data a dataframe to be formatted, either the aggregated stat table produced by key_stats(),
#' or another table to be formatted
#' @param column the name of a column within the dataframe, for data to be arranged by
#' @param col_alias the user-friendly name for the column in the output datatable
#' @param type either 'stat' if the input stat_data object is one created by key_stats(); 'word' if
#' it is a table created by get_top_n_words(); or 'emojis' if it is a dataframe showing most used emojis
#' 
#' @return a DT::datatable object to be surfaced in the app
stat_table <- function(stat_data, column, col_alias, type = "stat") {
  
  # Support tidyverse quoting
  column <- enquo(column)
  
  # Trim down dataframe and rename, depending on the type of data input
  if (type == "stat"){
  dat <- stat_data %>%
       arrange(desc(!!column)) %>%
       select("Sender" = sender,
              !!column,
              colour)
  names(dat) <- c("Sender", col_alias, "colour")
  } else if (type == "words") {
    dat <- stat_data %>%
       arrange(desc(!!column)) %>%
       select("Sender" = sender,
              word,
              colour,
              !!column)
  names(dat) <- c("Sender", "Word","colour", col_alias)
  } else if (type == "emojis") {
    dat <- stat_data %>%
       arrange(desc(!!column)) %>%
       select("Sender" = sender,
              emoji,
              colour,
              !!column)
  names(dat) <- c("Sender", "Emoji","colour", col_alias)
  }
  
  # After ensuring the dataframe is not empty, format as a datatable that can be shown to the user
  if (nrow(dat)>0) {
     dt <- dat %>%
       datatable(rownames = FALSE,
                 options = list(dom = 't',
                                ordering = FALSE,
                                columnDefs = list(list(visible = FALSE, targets = 2), # 'colour' column must be included for formatting but can stay hidden from user
                                                  list(className = 'dt-right', targets = 1:2),
                                                  list(className = 'dt-left', targets = 0)))) %>%
       formatStyle("colour",
                   target = "row",
                   color = styleEqual(dat$colour, dat$colour))
     return(dt)
  }
}

#' Function for extracting most used words for each sender
#' 
#' @param data the aliased dataframe
#' @param n number of words to show
#' @param stopwords flag for removing stopwords
#' 
#' @return dataframe of top n words for each sender
get_top_n_words <- function(data, n, stopwords, custom_remove, custom_search) {
  
  new_data <- data %>%
    filter(!image_flag,
           !video_flag,
           !gif_flag,
           !other_media_flag) %>%
    mutate(text = str_replace_all(text, pattern = "/", replacement = " "),
           text = str_replace_all(tolower(text), pattern = "[[:punct:]]", replacement = "")) %>%
    unnest_tokens(input = text,
                  output = "word",
                  token = "words")
  if (stopwords) {
    new_data <- new_data %>%
      filter(!word %in% tidytext::stop_words$word)
  }
  if (custom_remove!="") {
    custom_removers <- str_split(custom_remove, pattern = "[[:space:][:punct:]]+") %>%
      unlist()
    new_data <- new_data %>%
      filter(!word %in% tolower(custom_removers))
  }
  if (custom_search!="") {
    new_data <- new_data %>%
      filter(str_detect(word, pattern = custom_search))
  }
  new_data <- new_data %>%
    group_by(sender, colour, word) %>%
    summarise(num = n()) %>%
    arrange(desc(num)) %>%
    group_by(sender) %>%
    slice(1:n)
  return(new_data)
}