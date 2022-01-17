library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(performance)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
source("data-prep/overall/functions.R")
get_utts_w_target(childes_utterances, items)
aoa <- read_csv("data-prep/aoa/aoa.csv")

known_words <- utterances %>%
  filter(speaker_type == "other") %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
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
  filter(speaker_type == "other") %>%
  left_join(known_words, by = c("id")) %>%
  mutate(complexity = -log(known_prop), 
         complexity_scaled = scale(complexity),
         age_scaled = scale(age), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1))

m <- glmer(form_numeric ~ complexity_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = known_props, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/lexical-complexity.csv")