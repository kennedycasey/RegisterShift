library(childesr)
library(data.table)
library(tidyverse)

# get transcript ids and linked audio file names
transcripts <- get_transcripts(collection = "Eng-NA") %>%
  filter(target_child_age < 84) %>%
  mutate(audio_file = str_remove(gsub('.*\\/', '', filename), '.xml')) %>%
  select(transcript_id, corpus_name, target_child_name, audio_file)

# get timestamped utterances containing target word
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

utterances <- childes_utterances %>%
  filter(target_child_age < 84) %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

CDL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form == "CDL") %>%
  pull(word)

ads_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form == "ADL") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

colors <- c("ids" = "#C1292E", "ads" = "#235789")

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

timestamped <- data.frame(utterances) %>%
  filter(!is.na(media_start) & !is.na(media_end) & speaker_role!="Target_Child")

get_timestamped_utts <- list()

for (i in items){
  
  # TO DO: update so that this isn't necessary
  if (i == "night night"){
    subset <- timestamped %>%
      filter(!is.na(night.night)) %>%
      mutate(item = paste0(i)) %>%
      select(item, transcript_id, corpus_name, 
             target_child_name, media_start, media_end)
  }
  
  else subset <- timestamped %>%
    filter(!is.na(eval(as.symbol(i)))) %>%
    mutate(item = paste0(i)) %>%
    select(item, transcript_id, corpus_name, 
             target_child_name, media_start, media_end)
  
  get_timestamped_utts[[i]] <- subset
}

timestamped <- do.call(rbind, get_timestamped_utts) %>%
  left_join(transcripts, by = c("transcript_id", "corpus_name", "target_child_name"))
  
write_csv(timestamped, "data_prep/prosody/timestamped_utts.csv")