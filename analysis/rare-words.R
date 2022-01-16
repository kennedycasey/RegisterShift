library(tidyverse)
library(data.table)
library(childesr)
library(wordbankr)
library(lme4)
library(lmerTest)
source("data-prep/overall/functions.R")

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
get_utts_w_target(childes_utterances, items)

function_words <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(lexical_category == "function_words") %>%
  mutate(word = str_remove_all(uni_lemma, "(\\s*\\(\\w+\\))")) %>%
  pull(word)

`%notin%` <- Negate(`%in%`)

words <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(word %notin% function_words) %>%
  group_by(word) %>%
  summarize(raw_freq = n()) %>%
  mutate(row_number = row_number()) %>%
  filter(row_number != 1 & raw_freq > 1) %>%
  mutate(freq = scale(-raw_freq))

total <- sum(words$raw_freq)

relative_freq <- words %>%
  select(word, raw_freq) %>%
  mutate(freq = raw_freq/total, 
         freq = scale(-freq))

mean_freq <- utterances %>%
  filter(speaker_type == "other") %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(word %notin% function_words) %>%
  filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
  left_join(words, by = "word") %>%
  group_by(id) %>%
  summarize(freq = mean(freq, na.rm = TRUE))

freq_other <- utterances %>%
  filter(speaker_type == "other") %>%
  left_join(mean_freq, by = c("id")) %>%
  mutate(freq_scaled = scale(freq),
         age_scaled = scale(age)) %>%
  filter(!is.na(freq_scaled))

m <- glmer(form_numeric ~ freq_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = freq_other,
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/relative-freq.csv")