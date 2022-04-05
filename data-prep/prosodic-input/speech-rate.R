library(childesr)
library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)
source("data-prep/overall/get-min-dur.R")

utterances <- read_csv("data/childes-input.csv")

rate <- utterances %>%
  filter(!is.na(media_start) & 
           !is.na(media_end) & 
           (media_end - media_start) >= min_dur/1000) %>%
  mutate(rate = num_tokens/(media_end - media_start))

write_csv(rate, "data/input/rate.csv")