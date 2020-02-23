library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)
library(lubridate)

# Define functions for analysing Whatsapp data
clean_dataframe <- function(text_file_input_path) {
  
  # Read input file
  raw <- readLines(text_file_input_path)
  
  cat("\n..Beginning analysis...\n")
  cat("Stripping out non-messages and fixing multi-line messages...\n")
  if (str_detect(raw[1], pattern = "Messages to this chat and calls are now secured with end-to-end encryption.")) {
    raw <- raw[-1]
  }
  raw_vec <- raw
  for (i in rev(seq_along(raw_vec)[-1])) {
    if (str_detect(raw_vec[i], pattern = "^[^\\[]")) {
        raw_vec[i] <- substr(raw_vec[i], 2, nchar(raw_vec[i]))
      }
    if (!str_detect(raw_vec[i], pattern = "^\\[")) {
      raw_vec[i-1] <- paste0(raw_vec[i-1], " ", raw_vec[i])
      raw_vec[i] <- NA
    }
  }
  raw_vec2 <- raw_vec[!is.na(raw_vec)]
  
  # Transform into dataframe for easier use
  raw_df <- data.frame(raw_text = raw_vec2,
                       stringsAsFactors = FALSE) %>%
    mutate(date = substr(raw_text, 2, 11),
           time = substr(raw_text, 14, 21),
           stripped_text = substr(raw_text, 23, nchar(raw_text)),
           sender = str_extract(stripped_text, pattern = "[[:alnum:][:punct:][:space:]]+(?=: )"),
           text = str_extract(stripped_text, pattern = "(?<=: )[[:print:]]+$"),
           text = ifelse(str_detect(text, pattern = "(?<=^)[[:alnum:][:space:][:punct:]]"),
                         text,
                         substr(text, 2, nchar(text))),
           image_flag = text=="image omitted") %>%
    select(date, time, sender, text, image_flag)
  
}