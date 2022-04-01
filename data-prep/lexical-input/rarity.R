library(tidyverse)
library(data.table)
library(childesr)
library(wordbankr)
library(lme4)
library(lmerTest)

function_words <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(lexical_category == "function_words") %>%
  mutate(word = str_remove_all(uni_lemma, "(\\s*\\(\\w+\\))")) %>%
  pull(word)

`%notin%` <- Negate(`%in%`)

for (i in c("other", "child")) {
  utterances <- read_csv("data/childes-input.csv") %>%
    filter(speaker_type == i)
  
  if (i == "other") {
    words <- utterances %>%
      mutate(word = tolower(stem)) %>%
      separate_rows(word, sep = " ") %>%
      #filter(word %notin% function_words) %>%
      group_by(word) %>%
      summarize(raw_freq = n()) %>%
      mutate(row_number = row_number()) %>%
      filter(row_number != 1 & raw_freq >= 10)
  
    if (!file.exists("data-prep/lexical-input/raw-freq.csv")) {
      write_csv(words, "data-prep/lexical-input/raw-freq.csv")
    }
  }
    
  total <- sum(words$raw_freq)

  relative_freq <- words %>%
    select(word, raw_freq) %>%
    mutate(freq = raw_freq/total, 
          freq = -log(freq))

  rarity <- utterances %>%
    mutate(word = tolower(stem)) %>%
    separate_rows(word, sep = " ") %>%
    #filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
    left_join(relative_freq, by = "word") %>%
    group_by(id) %>%
    summarize(rarity = mean(freq, na.rm = TRUE))
  
  filename <- ifelse(i == "child", "data/input/rarity-child.csv", 
                     "data/input/rarity.csv")

  write_csv(rarity, filename)
}