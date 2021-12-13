library(wordbankr)
library(tidyverse)
library(ggrepel)

# get all unique words in cdi
get_items <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(type == "word")
items <- unique(get_items$item_id)

# get all administrations of eng ws cdi
eng_data <- get_instrument_data(language = "English (American)", form = "WS", items = items, 
                                administrations = TRUE, iteminfo = TRUE) %>%
  filter(!is.na(age))

# get aoa data
aoa_data <- fit_aoa(eng_data, measure = "understands", method = "glm", proportion = 0.5) 

aoa <- aoa_data %>%
  ungroup() %>%
  filter(!is.na(aoa)) %>%
  mutate(word = str_remove_all(uni_lemma, "(\\s*\\(\\w+\\))")) %>%
  select(word, aoa) %>%
  group_by(word) %>%
  summarize(aoa = mean(aoa))

write_csv(aoa, "data_prep/aoa.csv")
