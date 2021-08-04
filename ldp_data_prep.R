library(tidyverse)

# read in ldp data from hard-coded local path
raw_data <- read.csv("~/Desktop/secure/ldp_data.csv") 

# update to match childes data format
# TO DO: check compound word issue (+ and _)
# TO DO: deal with xxx
clean_data <- raw_data %>%
  select(subject, session, line, p_chat, c_chat) %>%
  filter(session <= 12) %>%
  mutate(age = session*4 + 10, #get age (in months) from session number
         p_transcription = trimws(tolower(str_replace_all(p_chat, "\\+|_", " "))), #remove leading/trailing whitespace, make lowercase, separate compound words
         c_transcription = trimws(tolower(str_replace_all(c_chat, "\\+|_", " "))),
         other_speaker = str_remove_all(p_transcription, regex("[^'[:lower:] ]")), #remove all punctuation except apostrophe
         target_child = str_remove_all(c_transcription, regex("[^'[:lower:] ]"))) %>%
  pivot_longer(c("other_speaker", "target_child"), names_to = "speaker", values_to = "transcription") %>%
  select(-p_chat, -c_chat, -p_transcription, -c_transcription) %>%
  filter(transcription != "") %>%
  mutate(num_tokens = str_count(transcription, "\\w+"),
         gloss = paste0(" ", transcription, " "))

write.csv(clean_data, "~/Desktop/secure/ldp_data_prepped.csv")