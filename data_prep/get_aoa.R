library(wordbankr)
library(tidyverse)
library(ggrepel)

# get all unique words in cdi
get_items <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(type == "word")
items <- unique(get_items$item_id)
item_ids <- get_items %>%
  mutate(word = str_remove_all(uni_lemma, "(\\s*\\(\\w+\\))")) %>%
  select(num_item_id, word)

# get all administrations of eng ws cdi
eng_data <- get_instrument_data(language = "English (American)", form = "WS", items = items, 
                                administrations = TRUE, iteminfo = TRUE) %>%
  filter(!is.na(age))

# get aoa data
aoa_data <- fit_aoa(eng_data, measure = "understands", 
                    method = "glm", proportion = 0.5) 

aoa <- aoa_data %>%
  ungroup() %>%
  filter(!is.na(aoa)) %>%
  mutate(word = str_remove_all(uni_lemma, "(\\s*\\(\\w+\\))")) %>%
  select(word, aoa) %>%
  group_by(word) %>%
  summarize(aoa = mean(aoa))

write_csv(aoa, "data_prep/aoa.csv")

# get production percentiles
production_data <- get_instrument_data(language = "English (American)", 
                                       form = "WS", items = items, 
                                       administrations = TRUE) 

production_percentile <- production_data %>%
  select(age, num_item_id, value) %>%
  mutate(known = case_when(
    value == "produces" ~ "known", 
    value != "produces" | is.na(value) ~ "not_known")) %>%
  left_join(item_ids, by = "num_item_id") %>%
  group_by(word, age, known) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = known, values_from = n) %>%
  mutate(known = ifelse(is.na(known), 0, known), 
         not_known = ifelse(is.na(not_known), 0, not_known),
         prod_prop = known/(known + not_known))

write_csv(production_percentile, "data_prep/prod_norms.csv")

