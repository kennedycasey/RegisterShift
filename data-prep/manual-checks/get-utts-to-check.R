library(tidyverse)
library(vroom)
path <- "data-prep/prosodic-input/processed/"
files <- list.files(path, ".csv")
files <- lapply(paste0(path, files), vroom)

raw.data <- do.call(rbind, files)
set.seed(1)

data <- raw.data %>%
  mutate(form = case_when(
    form == "ADS" ~ "ADL", 
    form == "CDS" ~ "CDL")) %>%
  # remove rows that are already not included in analysis
  mutate(across(starts_with("pitch"), ~as.numeric(as.character(.))), 
         # correct for missing leading zeroes
         audio_file = ifelse(corpus_name %in% c("Brent", "Soderstrom") & nchar(audio_file) == 4, 
                             paste0("00", audio_file), 
                             ifelse(corpus_name %in% c("Brent", "Soderstrom") & nchar(audio_file) == 3, 
                                    paste0("000", audio_file), 
                                    ifelse(corpus_name %in% c("Brent", "Soderstrom") & nchar(audio_file) == 5, 
                                           paste0("0", audio_file), audio_file))),
         audio_file = ifelse(!is.na(as.numeric(as.character(substr(audio_file, 1, 1)))) & 
                               as.numeric(as.character(substr(audio_file, 1, 1))) != 0 & 
                               corpus_name != "NewmanRatner", 
         paste0("0", audio_file), audio_file)) %>%
  filter(!is.na(pitch_range) & !is.na(pitch_min) & !is.na(pitch_max) 
         & !is.na(pitch_mean)) %>%
  # create var for 4 different subsets
  mutate(subset = paste0(speaker_type, "-", form))
 
ffmpeg_list <- list()
mappings_list <- list()
counts_list <- list()
for (i in unique(data$subset)) {
  for (word in unique(data$item)) {
    
  d <- data %>%
    filter(subset == i & item == word)
  
  n <- round(nrow(d)*.15)
  
  if (n > 0) {
  sample <- slice_sample(d, n = n) %>%
    mutate(filename = paste0(speaker_type, "-", str_remove(word, " "), row_number()))
  
  mappings_list[[word]] <- sample
  counts_list[[word]] <- sample %>%
    mutate(count = n()) %>%
    select(item, count) %>%
    distinct()
  
  sample_ffmpeg <- sample %>%
    mutate(input = paste0("ffmpeg -i ", corpus_name, "/", target_child_name, 
                          "/", audio_file, ".wav -ss ", media_start, 
                          " -to ", media_end, " _ready_for_annotation/", filename, ".wav")) %>%
    select(input)
  
  ffmpeg_list[[word]] <- sample_ffmpeg

  sample_anotar <- sample %>%
    rename(transcription = gloss, 
           word = item,
           speaker = speaker_type) %>%
    select(filename, transcription, word, speaker)
  
  filename <- paste0("data-prep/manual-checks/sampled-utts/", 
                            i, "-", str_remove(word, " "), ".csv")
  
  write_csv(sample_anotar, filename)
  }
  }
  
  ffmpeg <- do.call(rbind, ffmpeg_list)
  write.table(ffmpeg, file = paste0("data-prep/manual-checks/get-audio-clips-", i, ".txt"), quote = FALSE, 
              sep = "/t", row.names = FALSE , col.names = FALSE)

  mappings <- do.call(rbind, mappings_list)
  write_csv(mappings, paste0("data-prep/manual-checks/full-mappings-", i, ".csv"))
  counts <- do.call(rbind, counts_list)
  write_csv(counts, paste0("data-prep/manual-checks/counts-", i, ".csv"))
  
  # re-initialize empty lists
  ffmpeg_list <- list()
  mappings_list <- list()
  counts_list <- list()
}