library(data.table)
library(tidyverse)

utterances <- read_csv("~/Desktop/secure/ldp_data_prepped.csv")

items <- read_csv("data-prep/overall/item-info.csv") %>%
  pull(word)

pairs <- read_csv("data-prep/overall/item-info.csv") %>%
  select(word, pair)

colors <- c("CDS" = "#C1292E", "ADS" = "#235789")

CDS_forms <- read_csv("data-prep/overall/item-info.csv") %>%
  filter(form == "CDS") %>%
  pull(word)

ADS_forms <- read_csv("data-prep/overall/item-info.csv") %>%
  filter(form == "ADS") %>%
  pull(word)

utts_list <- list()

for (i in items) {
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
      
    utts_w_target <- utterances %>%
      filter(str_detect(gloss,
                        regex(paste0(" ",
                                     root, "ey | ", 
                                     root, "ie | ",
                                     root, "eys | ", 
                                     root, "ies | ",
                                     root, "ey's | ", 
                                     root, "ie's "))))}
    
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
      
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, 
                          regex(paste0(" ", 
                                      root, "y | ", 
                                      root, "ie | ",
                                      root, "ys | ", 
                                      root, "ies | ",
                                      root, "y's | ", 
                                      root, "ie's "))))}
    
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
      
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, 
                        regex(paste0(" ",
                                     root, "y | ",
                                     root, "ie | ",
                                     root, "ys | ",
                                     root, "ies | ",
                                     root, "y's | ",
                                     root, "ie's "))))}
    
  else if (i == "night night") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, 
                        regex(paste0(" night night | night-night | ",
                                     "night nights | night-nights "))))}
  else if (i == "goodnight") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss,
                        regex(paste0(" goodnight | good night | good-night "))))}
    
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss,
                        regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))))
    
  utts_list[[i]] <- utts_w_target %>%
    mutate(item = paste0(i),
           form = case_when(
             i %in% CDS_forms ~ "CDS", 
             i %in% ADS_forms ~ "ADS"), 
           pair = paste0((filter(pairs, word == i))$pair))
}
  
utterances <- do.call(rbind, utts_list) %>%
  mutate(form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1), 
         speaker_type = case_when(
           speaker == "target_child" ~ "child", 
           speaker != "target_child" ~ "other"))

for (i in items) {
  subset <- utterances %>%
    filter(item == i) %>%
    select(-1)
  
  path <- "~/Desktop/secure/ldp-byword/"
  
  if (i == "night night") {
    write_csv(subset, paste0(path, "night-night.csv"))
  } else {
    write_csv(subset, paste0(path, i, ".csv")) 
  }
}