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

utterances <- read_csv("data/childes-input.csv") %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, stem)

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

# check NAs
complexity_nas <- utterances %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  #filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
  left_join(aoa, by = "word") %>%
  mutate(na = ifelse(is.na(aoa), "na", "not_na")) %>%
  group_by(id, na) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "na", values_from = "n") %>%
  mutate(na = ifelse(is.na(na), 0, na), 
         not_na = ifelse(is.na(not_na), 0, not_na)) %>%
  group_by(id) %>%
  summarize(na_prop = na/(na + not_na))
mean(complexity_nas$na_prop)