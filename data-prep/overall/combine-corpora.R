library(tidyverse) 

# read in CHILDES data
childes <- read_csv("data/childes-input.csv") %>%
  mutate(id = as.character(id), 
         speaker_id = as.character(speaker_id), 
         target_child_id = as.character(target_child_id)) %>%
  rename(length = num_tokens) %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, length)

# read in LDP data and update to match CHILDES cols
ldp <- read_csv("~/Desktop/secure/ldp-input.csv") %>%
  mutate(corpus_name = "LDP", 
         target_child_id = paste0("LDP", target_child_id), 
         speaker_id = paste0(target_child_id, "_", speaker_type), 
         id = paste0(target_child_id, "_", line)) %>%
  rename(length = num_tokens) %>%
  select(corpus_name, id, speaker_id, target_child_id, age, 
         pair, item, form, form_numeric, length)

# read in input predictor data
complexity <- read_csv("data/input/complexity.csv")

input <- bind_rows(childes, ldp) %>%
  left_join(complexity, by = c("corpus_name", "id",
                               "speaker_id", "target_child_id", 
                               "age", "pair", "item", 
                               "form", "form_numeric"))
write_csv(input, "data/full-input.csv")