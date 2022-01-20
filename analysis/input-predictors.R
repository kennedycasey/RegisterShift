library(tidyverse)
library(lme4)
library(broom.mixed)

pitch <- read_csv("data/input/pitch.csv")
rate <- read_csv("data/input/rate.csv")
complexity <- read_csv("data/input/complexity.csv")
rarity <- read_csv("data/input/rarity.csv")
verbs <- read_csv("data/input/verbs.csv")
length <- read_csv("data/input/length.csv")

# model effect of mean pitch on form -> sing fit
m.pitch.mean <- glmer(form_numeric ~ pitch_mean_scaled * age_scaled + 
             (pitch_mean_scaled * age_scaled|pair) + 
             (pitch_mean_scaled * age_scaled|speaker_id), 
           data = pitch, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)

tidy(m.pitch.mean) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/input/mean-pitch.csv")

m.pitch.range <- glmer(form_numeric ~ pitch_range_scaled * age_scaled + 
             (pitch_range_scaled * age_scaled|pair) + 
             (pitch_range_scaled * age_scaled|speaker_id), 
           data = pitch_info, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m.pitch.range)

tidy(m.pitch.range) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/input/pitch-range.csv")