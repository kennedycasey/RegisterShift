library(tidyverse)
library(data.table)
library(childesr)

items <- read_csv("../../data_prep/item_info.csv") %>%
  pull(word)

cds_forms <- read_csv("../../data_prep/item_info.csv") %>%
  filter(form == "CDS") %>%
  pull(word)

ads_forms <- read_csv("../../data_prep/item_info.csv") %>%
  filter(form == "ADS") %>%
  pull(word)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

child_utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role == "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

other_utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# get overall token counts
get_freq <- list()
for (speaker_type in c("child_utterances", "other_utterances")) {
  utterances <- eval(as.symbol(speaker_type))
  for (i in items) {
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
    
    else if (i == "night night") {
      utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
                 paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
    }
    
    else if (i == "goodnight") {
      utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
                 paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
    }
    
    else utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                    paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
  }
  
  utts_w_target <- utterances[, 28:ncol(utterances)]
  utts_w_target[is.na(utts_w_target)] <- 0
  freq <- data.frame(colSums(utts_w_target))
  setNames(cbind(rownames(freq), freq, row.names = NULL), c("word", "freq")) -> get_freq[[speaker_type]]
}

freq <- do.call(cbind, get_freq) %>%
  select(-other_utterances.word) %>%
  rename(word = child_utterances.word, 
         child = child_utterances.freq, 
         other = other_utterances.freq) %>%
  filter(word != "age")

write_csv(freq, "freq.csv")