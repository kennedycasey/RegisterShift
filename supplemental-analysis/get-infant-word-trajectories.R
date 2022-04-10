library(childesr)
library(data.table)
library(tidyverse)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

utterances <- childes_utterances %>%
  filter(target_child_age < 84) %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

items <- c("pacifier", "peekaboo", "diaper", "high chair", 
           "sippy cup", "bib", "baby", "bottle")

utts_list <- list()

for (i in items) {
  if (i == "peekaboo") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" peekaboo | peek a boo | peek-a-boo | peekaboos | peek a boos | peek-a-boos "))))
  }
  
  else if (i == "high chair") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" high chair | highchair | high+chair | high chairs | highchairs | high+chairs "))))
  }
  
  else if (i == "sippy cup") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" sippy cup | sippy+cup | sippy cups | sippy+cups "))))
  }
  
  else if (i == "baby") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" baby | babies | baby's "))))
  }
  
  else utts_w_target <- utterances %>%
    filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s "))))
    
  utts_list[[i]] <- utts_w_target %>%
    mutate(item = paste0(i))
}
  
utterances <- do.call(rbind, utts_list) %>%
  mutate(speaker_type = case_when(
           speaker_role == "Target_Child" ~ "child", 
           speaker_role != "Target_Child" ~ "other"))

utterances %>%
  group_by(item, age) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = age, y = n, group = item)) + 
  geom_smooth(method = "loess", se = FALSE, color = "#FF595E") +
  geom_point(color = "#FF595E", alpha = 0.2) + 
  labs(x = "Age (months)", y = "Raw freqeuency of other infant-relevant words") +
  theme_test(base_size = 15) 

ggsave("supplemental-analysis/figs/infant-words.jpg", dpi = 300, width = 5, height = 5)
