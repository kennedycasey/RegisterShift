library(tidyverse) 

# read in CHILDES data
childes <- read_csv("data/childes-input.csv") %>%
  mutate(id = paste0(id), 
         speaker_id = paste0(speaker_id), 
         target_child_id = paste0(target_child_id))

# read in LDP data and update to match CHILDES cols
ldp <- read_csv("~/Desktop/secure/ldp-input.csv") %>%
  mutate(corpus_name = "LDP", 
         target_child_id = paste0("LDP", target_child_id), 
         speaker_id = paste0(target_child_id, "_", speaker_type), 
         id = paste0(target_child_id, "_", line))

# read in input predictor data
input <- bind_rows(childes, ldp)
write_csv(input, "data/full-input.csv")