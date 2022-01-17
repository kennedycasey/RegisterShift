library(tidyverse)
library(lme4)
library(lmerTest)
source("data-prep/overall/functions.R")

# read in pitch info from data_prep folder
# merge into single df
path_to_pitch_info = "data-prep/prosody/processed/"
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
         age_scaled = scale(age))

m <- glmer(form_numeric ~ pitch_mean_scaled * age_scaled + 
             (1|pair) + (1|speaker_id), 
           data = pitch_info, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/mean-pitch.csv")

m <- glmer(form_numeric ~ pitch_range_scaled * age_scaled + 
             (1|pair) + (1|speaker_id), 
           data = pitch_info, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/pitch-range.csv")