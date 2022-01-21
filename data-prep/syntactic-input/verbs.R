library(childesr)
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)

path_to_verb_info = "data-prep/syntactic-input/parsed/"
parsed_filenames <- list.files(path = path_to_verb_info, pattern = "*.csv")
verb_files <- lapply(paste0(path_to_verb_info, parsed_filenames), read_csv)
verb_info <- do.call(rbind, verb_files) %>%
  mutate(verbs = str_count(parsed_gloss, fixed("(VP ")) +
           str_count(parsed_gloss, fixed("(VBP ")) -
           str_count(parsed_gloss, fixed("(VP (VBP "))) %>%
  distinct() %>%
  filter(speaker_type == "other") %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, verbs)

write_csv(verb_info, "data/input/verbs.csv")