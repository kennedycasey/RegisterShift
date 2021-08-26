library(childesr)
library(tidyverse)

transcripts <- get_transcripts(collection = "Eng-NA") %>%
  filter(target_child_age < 84) %>%
  mutate(audio_file = str_remove(gsub('.*\\/', '', filename), '.xml')) %>%
  select(transcript_id, corpus_name, audio_file)