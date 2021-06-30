library(wordbankr)
library(tidyverse)
library(readr)


wg_all <- get_instrument_data(language = "English (American)", 
                          form = "WG", 
                          iteminfo = TRUE)

wg <- wg_all %>%
  filter(type == "word") %>%
  select(definition) %>%
  transmute(word = definition) %>%
  distinct() 

# write_csv(wg, "wg_items.csv")


ws_all <- get_instrument_data(language = "English (American)", 
                              form = "WS", 
                              iteminfo = TRUE)

ws <- ws_all %>%
  filter(type == "word") %>%
  select(definition) %>%
  transmute(word = definition) %>%
  distinct() 

# write_csv(ws, "ws_items.csv")
