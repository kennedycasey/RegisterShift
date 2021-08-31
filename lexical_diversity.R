library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances <- read.csv("~/Desktop/secure/ldp_data_prepped.csv") 

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

aoa <- read_csv("data_prep/item_info.csv") %>%
  select(word, aoa, pair, form)

colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

CDL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="CDL") %>%
  pull(word)

ADL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="ADL") %>%
  pull(word)


# CHILDES -----------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# create empty list to be populated
get_utts <- list() 
# loop over all items to get num_tokens for all utterances containing a target word
for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_utts[[i]] <- utts_w_target
}

utts <- do.call(rbind, get_utts)

iterations <- utterances %>%
  select(transcript_id, speaker_id) %>%
  distinct() %>%
  mutate(index = row_number())

get_order_info <- list()
for(i in unique(iterations$index)) {
  transcript_id_value <- (filter(iterations, index == i))$transcript_id
  speaker_id_value <- (filter(iterations, index == i))$speaker_id
  
  transcript_by_speaker <- filter(utterances, transcript_id == transcript_id_value & speaker_id == speaker_id_value) %>%
    mutate(utterance = row_number())
  
  get_order_info[[i]] <- transcript_by_speaker
}
ordered_utts <- do.call(rbind, get_order_info)

merged_utts <- ordered_utts %>%
  select(id, utterance, speaker_id) %>%
  right_join(utts, by = c("id")) %>%
  select(id, gloss, num_tokens, transcript_id, speaker_id, 
         speaker_role, target_child_id, age, item, pair, form, utterance) %>%
  mutate(gloss_grouped = paste(trimws((filter(ordered_utts, utterance == preceding & transcript_id == transcript_id & speaker_id == speaker_id))$gloss),
                               trimws(gloss), 
                               trimws((filter(ordered_utts, utterance == following & transcript_id == transcript_id & speaker_id == speaker_id))$gloss)), 
         gloss_grouped = trimws(str_remove(gloss_grouped, "xxx|yyy")))