library(childesr)
library(data.table)
library(tidyverse)

# get transcript ids and linked audio file names
raw_transcripts <- get_transcripts(collection = "Eng-NA") %>%
  filter(target_child_age < 84) %>%
  mutate(audio_file = str_remove(gsub('.*\\/', '', filename), '.xml'))

# fix individual corpora from metadata
## rm target child last names
brent_transcripts <- raw_transcripts %>%
  filter(corpus_name == "Brent") %>%
  select(transcript_id, corpus_name, target_child_name, audio_file) %>%
  mutate(target_child_name = ifelse(
    str_detect(target_child_name, "_"), str_remove(target_child_name, regex("_[^_]+$")), 
    target_child_name))

## replace audio files to avoid double indexing
gleason <- read_csv("data-prep/prosodic-input/childes-audio-matching/gleason.csv") %>%
  select(corpus_name, target_child_name, pid, audio_file)

gleason_transcripts <- raw_transcripts %>%
  filter(corpus_name == "Gleason") %>%
  select(-audio_file) %>%
  left_join(gleason, by = c("corpus_name", "target_child_name", "pid")) %>%
  select(transcript_id, corpus_name, target_child_name, audio_file)

## fix target child names and audio file names
newmanratner_transcripts <- raw_transcripts %>%
  filter(corpus_name == "NewmanRatner") %>%
  mutate(target_child_name = audio_file,
         audio_file = paste0(audio_file, "-", 
                             str_remove_all(
                               str_extract(filename, '/([0-9][0-9])/'), '/'))) %>%
  mutate(target_child_name = ifelse(target_child_name == "5224EZ", "5224EZS", 
                                    ifelse(target_child_name == "577LBE", "577LE", 
                                           target_child_name))) %>%
  select(transcript_id, corpus_name, target_child_name, audio_file) 

## add target child names
rollins <- read_csv("data-prep/prosodic-input/childes-audio-matching/rollins.csv")

rollins_transcripts <- raw_transcripts %>%
  filter(corpus_name == "Rollins") %>%
  select(-target_child_name) %>%
  left_join(rollins, by = c("corpus_name", "audio_file")) %>%
  select(transcript_id, corpus_name, target_child_name, audio_file)


# merge all corpora
`%notin%` <- Negate(`%in%`)
transcripts <- raw_transcripts %>%
  filter(corpus_name %notin% c("Brent", "Gleason", "NewmanRatner", "Rollins")) %>%
  select(transcript_id, corpus_name, target_child_name, audio_file) %>%
  bind_rows(brent_transcripts, gleason_transcripts, newmanratner_transcripts, rollins_transcripts)

# get timestamped utterances containing target word
timestamped <- read_csv("data/childes-input.csv") %>%
  filter(!is.na(media_start) & !is.na(media_end))

items <- read_csv("data-prep/overall/item-info.csv") %>%
  pull(word)

for (i in items) {
  utterances <- timestamped %>%
    filter(item == i) %>%
    select(-target_child_name) %>%
    left_join(transcripts, by = c("transcript_id", "corpus_name")) %>%
    # add empty columns to be populated when running praat script
    mutate(pitch_mean = "", 
           pitch_min = "", 
           pitch_max = "", 
           pitch_range = "")
  
  write_csv(utterances, paste0("data-prep/prosodic-input/timestamped/", i, ".csv"))
}