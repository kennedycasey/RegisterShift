library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)

# get all utterances from providence corpus
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

utterances <- childes_utterances %>%
  filter(target_child_age < 60) %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

items <- read_csv("data_prep/item_info/candidate_items_new.csv") %>%
  pull(word)

ids_forms <- read_csv("data_prep/item_info/candidate_items_new.csv") %>%
  filter(form == "ids") %>%
  pull(word)

ads_forms <- read_csv("data_prep/item_info/candidate_items_new.csv") %>%
  filter(form == "ads") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info/candidate_items_new.csv") %>%
  select(word, pair)

colors <- c("ids" = "#C1292E", "ads" = "#235789")

# loop over all items to get speech rate for all utterances containing a target word
for(i in items){
  
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                              root, "eys | ", root, "ies | ",
                                              root, "ey's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                          root, "eys | ", root, "ies | ",
                                                          root, "ey's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (i == "night night"){
    utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
               paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
  }
  
  else if (i == "goodnight"){
    utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
               paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                  paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
}

timestamped <- utterances
timestamped <- timestamped %>%
  filter(!is.na(media_start) & !is.na(media_end) & speaker_role!="Target_Child") %>%
  #test on providence for now
  filter(corpus_name=="Providence")

get_timestamped_utts <- list()
for (i in items){
  subset <- timestamped %>%
    filter(!is.na(eval(as.symbol(i)))) %>%
    mutate(item = paste0(i)) %>%
    select(item, id, transcript_id, media_start, media_end)
  
  get_timestamped_utts[[i]] <- subset
}
timestamped <- do.call(rbind, get_timestamped_utts)