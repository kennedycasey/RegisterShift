library(tidyverse)
library(lme4)
library(broom.mixed)

pitch <- read_csv("data/input/pitch.csv")

input <- read_csv("data/full-input.csv") %>%
  mutate(age_scaled = scale(age), 
         length_scaled = scale(length), 
         complexity_scaled = scale(complexity))

# no effect of mean pitch on form
m.pitch.mean <- glmer(form_numeric ~ pitch_mean_scaled * age_scaled + 
                        (pitch_mean_scaled|pair) + 
                        (age_scaled|pair) +
                        (pitch_mean_scaled|speaker_id) + 
                        (age_scaled|speaker_id),
                      data = pitch, 
                      family = binomial, 
                      control = glmerControl(optimizer = "bobyqa"))
summary(m.pitch.mean)

tidy(m.pitch.mean) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/input/mean-pitch.csv")

# marginal effect of pitch range on form
m.pitch.range <- glmer(form_numeric ~ pitch_range_scaled * age_scaled + 
                         (pitch_range_scaled|pair) + 
                         (age_scaled|pair) +
                         (pitch_range_scaled|speaker_id) + 
                         (age_scaled|speaker_id),
                       data = pitch_info, 
                       family = binomial, 
                       control = glmerControl(optimizer = "bobyqa"))
summary(m.pitch.range)

tidy(m.pitch.range) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/input/pitch-range.csv")

# sig effect of lexical complexity on form
m.complexity <- glmer(form_numeric ~ complexity_scaled * age_scaled + 
                        (complexity_scaled|pair) + 
                        (age_scaled|pair) +
                        (complexity_scaled|speaker_id) + 
                        (age_scaled|speaker_id),
                      data = input, 
                      family = binomial, 
                      control = glmerControl(optimizer = "bobyqa"))
summary(m.complexity)

tidy(m.complexity) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/input/complexity.csv")

# sig effect of utterance length on form
m.length <- glmer(form_numeric ~ length_scaled * age_scaled + 
                    (length_scaled|pair) + 
                    (age_scaled|pair) +
                    (length_scaled|speaker_id) + 
                    (age_scaled|speaker_id),
                  data = input, 
                  family = binomial, 
                  control = glmerControl(optimizer = "bobyqa"))
summary(m.length)

tidy(m.length) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/input/length.csv")