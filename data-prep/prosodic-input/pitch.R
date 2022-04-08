library(tidyverse)
library(lme4)
library(lmerTest)
source("data-prep/overall/functions.R")

# read in pitch info from data_prep folder
# merge into single df
path_to_pitch_info = "data-prep/prosodic-input/processed/"
pitch_filenames <- list.files(path = path_to_pitch_info, pattern = ".csv")
pitch_files <- lapply(paste0(path_to_pitch_info, pitch_filenames), read_csv)

i = "child"
for (i in c("other", "child")) {
  pitch_info <- do.call(rbind, pitch_files) %>%
    filter(speaker_type == i) %>%
    mutate(form = case_when(
        item %in% CDL_forms ~ "CDL", 
        item %in% ADL_forms ~ "ADL")) %>%
    filter(across(starts_with("pitch"), ~ . != "audio file missing" &
                    . != "relevant audio clip missing" &
                    . != "utterance too short to analyze" &
                    . != "--undefined--" &
                    . != "problematic line")) %>%
    mutate(across(starts_with("pitch"), ~ as.numeric(as.character(.)))) %>%
    group_by(item) %>%
    filter(media_end - media_start > 0.5) %>%
    mutate(form = factor(form, levels = c("CDL", "ADL")), 
           form_numeric = case_when(
             form == "CDL" ~ 0, 
             form == "ADL" ~ 1), 
           pitch_range = abs(pitch_range))
  
  filename <- ifelse(i == "child", "data/input/pitch-child.csv", 
                     "data/input/pitch.csv")
  
  write_csv(as.data.frame(as.matrix(pitch_info)), filename)
}