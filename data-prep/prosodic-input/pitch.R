library(tidyverse)
library(lme4)
library(lmerTest)
source("data-prep/overall/functions.R")

# read in pitch info from data_prep folder
# merge into single df
path_to_pitch_info = "data-prep/prosodic-input/processed/"
pitch_filenames <- list.files(path = path_to_pitch_info, pattern = "*.csv")
pitch_files <- lapply(paste0(path_to_pitch_info, pitch_filenames), read_csv)
pitch_info <- do.call(rbind, pitch_files) %>%
  mutate(form = case_when(
      item %in% CDS_forms ~ "CDS", 
      item %in% ADS_forms ~ "ADS")) %>%
  left_join(pairs %>% rename(item = word), by = "item") %>%
  filter(across(starts_with("pitch"), ~ . != "audio file missing" &
                  . != "relevant audio clip missing" &
                  . != "utterance too short to analyze" &
                  . != "--undefined--")) %>%
  mutate(across(starts_with("pitch"), ~ as.numeric(as.character(.)))) %>%
  group_by(item) %>%
  filter(media_end - media_start > 0.5) %>%
  mutate(form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1), 
         pitch_range = abs(pitch_range),
         pitch_mean_scaled = scale(pitch_mean), 
         pitch_range_scaled = scale(pitch_range),
         age_scaled = scale(age)) %>%
  select(item, pair, form, form_numeric, speaker_id, target_child_id, 
         age, age_scaled, pitch_mean, pitch_mean_scaled, pitch_range, 
         pitch_range_scaled)

write_csv(as.data.frame(as.matrix(pitch_info)), "data/input/pitch.csv")