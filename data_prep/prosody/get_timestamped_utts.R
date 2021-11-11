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
gleason <- read_csv("data_prep/prosody/childes-audio-matching/gleason.csv") %>%
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
rollins <- read_csv("data_prep/prosody/childes-audio-matching/rollins.csv")

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
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

utterances <- childes_utterances %>%
  filter(target_child_age < 84) %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

for(i in items){
  
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                              root, "eys | ", root, "ies | ",
                                              root, "ey's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                          root, "eys | ", root, "ies | ",
                                                          root, "ey's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (i == "night night"){
    utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
               paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
  }
  
  else if (i == "goodnight"){
    utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
               paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                  paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
}

timestamped <- data.frame(utterances) %>%
  filter(!is.na(media_start) & !is.na(media_end) & speaker_role!="Target_Child")

get_timestamped_utts <- list()

for (i in items){
  
  # TO DO: update so that this isn't necessary
  if (i == "night night"){
    subset <- timestamped %>%
      filter(!is.na(night.night)) %>%
      mutate(item = paste0(i)) %>%
      select(item, transcript_id, corpus_name, speaker_id,
             age, target_child_name, target_child_id, 
             media_start, media_end)
  }
  
  else subset <- timestamped %>%
    filter(!is.na(eval(as.symbol(i)))) %>%
    mutate(item = paste0(i)) %>%
      select(item, transcript_id, corpus_name, speaker_id,
             age, target_child_name, target_child_id, 
             media_start, media_end)
  
  get_timestamped_utts[[i]] <- subset
}

for(i in items) {
  timestamped <- do.call(rbind, get_timestamped_utts) %>%
    filter(item == i) %>%
    select(-target_child_name) %>%
    left_join(transcripts, by = c("transcript_id", "corpus_name")) %>%
    # add empty columns to be populated when running praat script
    mutate(pitch_mean = "", 
           pitch_min = "", 
           pitch_max = "", 
           pitch_range = "")
  
  write_csv(timestamped, paste0("data_prep/prosody/timestamped/", i, ".csv"))
}