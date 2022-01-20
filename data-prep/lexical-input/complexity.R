library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(performance)
source("data-prep/overall/functions.R")

aoa <- read_csv("data-prep/lexical-input/aoa.csv")

childes <- read_csv("data/childes-input.csv") %>%
  mutate(id = as.character(id), 
         speaker_id = as.character(speaker_id), 
         target_child_id = as.character(target_child_id)) %>%
  rename(length = num_tokens) %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, stem)

# read in LDP data and update to match CHILDES cols
ldp <- read_csv("~/Desktop/secure/ldp-input.csv") %>%
  mutate(corpus_name = "LDP", 
         target_child_id = paste0("LDP", target_child_id), 
         speaker_id = paste0(target_child_id, "_", speaker_type), 
         id = paste0(target_child_id, "_", line), 
         stem = gloss) %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, stem)

utterances <- bind_rows(childes, ldp)

known_words <- utterances %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  #filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
  left_join(aoa, by = "word") %>%
  mutate(known = case_when(
    !is.na(aoa) & aoa <= age ~ "known",
    !is.na(aoa) & aoa > age ~ "not_known")) %>%
  filter(!is.na(known)) %>%
  group_by(id, known) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "known", values_from = "n") %>%
  group_by(id) %>%
  summarize(known_prop = known/(known + not_known))

known_props <- utterances %>%
  left_join(known_words, by = c("id")) %>%
  mutate(complexity = -log(known_prop)) %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, complexity)

write_csv(known_props, "data/input/complexity.csv")