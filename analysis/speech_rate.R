library(childesr)
library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)

# get timestamped utterances containing target word
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

CDS_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form == "CDS") %>%
  pull(word)

ADS_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form == "ADS") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

colors <- c("ids" = "#C1292E", "ADS" = "#235789")

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
  filter(!is.na(media_start) & !is.na(media_end) & speaker_role != "Target_Child")

get_timestamped_utts <- list()

for (i in items){
  
  # TO DO: update so that this isn't necessary
  if (i == "night night"){
    subset <- timestamped %>%
      filter(!is.na(night.night)) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair)) %>%
      select(item, transcript_id, id, corpus_name, speaker_id,
             age, target_child_name, media_start, media_end,
             num_tokens, form, pair)
  }
  
  else subset <- timestamped %>%
    filter(!is.na(eval(as.symbol(i)))) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair)) %>%
      select(item, transcript_id, id, corpus_name, speaker_id,
             age, target_child_name, media_start, media_end,
             num_tokens, form, pair)
  
  get_timestamped_utts[[i]] <- subset
}

timestamped <- do.call(rbind, get_timestamped_utts) %>%
  filter(media_end - media_start > 0.5) %>%
  mutate(form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1), 
         speech_rate = num_tokens/(media_end - media_start), 
         speech_rate_scaled = scale(speech_rate), 
         age_scaled = scale(age))

write.csv(data.frame(timestamped), "speech_rate.csv", row.names = FALSE)

m <- glmer(form_numeric ~ speech_rate_scaled * age_scaled + (1 + speech_rate_scaled * age_scaled|pair) + (1 + speech_rate_scaled * age_scaled|speaker_id), 
           data = timestamped, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)

for(i in items) {
  timestamped <- do.call(rbind, get_timestamped_utts) %>%
    filter(item == i) %>%
    left_join(transcripts, by = c("transcript_id", "corpus_name", "target_child_name")) %>%
    # add empty columns to be populated when running praat script
    mutate(pitch_mean = "", 
           pitch_min = "", 
           pitch_max = "", 
           pitch_range = "")
  
  write_csv(timestamped, paste0("data_prep/prosody/timestamped/", i, ".csv"))
}