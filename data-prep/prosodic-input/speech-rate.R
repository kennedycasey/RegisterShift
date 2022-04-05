library(childesr)
library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)
source("data-prep/overall/get-min-dur.R")

for (i in c("other", "child")) {
  rate <- read_csv("data/childes-input.csv") %>%
    filter(speaker_type == i &
             !is.na(media_start) & 
             !is.na(media_end) & 
             (media_end - media_start) >= min_dur/1000) %>%
    mutate(rate = num_tokens/(media_end - media_start))
  
  filename <- ifelse(i == "child", "data/input/rate-child.csv", 
                     "data/input/rate.csv")
  
  write_csv(rate, filename)
}