library(wordbankr)
library(tidyverse)
library(ggrepel)

#get all unique words in cdi
get_items <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(type=="word")
items <- unique(get_items$item_id)

#get all administrations of eng ws cdi
eng_data <- get_instrument_data(language = "English (American)", form = "WS", items = items, 
                                administrations = TRUE, iteminfo = TRUE) %>%
  filter(!is.na(age))

#get aoa data
aoa_data <- fit_aoa(eng_data, measure = "produces", method = "glm", proportion = 0.5) 

aoa <- aoa_data %>%
  ungroup() %>%
  select(definition, aoa) %>%
  rename(word = definition) %>%
  write_csv("aoa.csv")

babiness <- read_csv("babiness.csv")

merged <- merge(aoa, babiness, by="word", keep = TRUE)