library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)

providence.kids <- c(23471, 23487, 23495, 23510, 23522, 23530)

pitch <- read_csv("data/input/pitch.csv") %>%
  filter(target_child_id %in% providence.kids) %>%
  mutate(age_scaled = scale(age), 
         pitch_mean_scaled = scale(pitch_mean), 
         pitch_range_scaled = scale(pitch_range))

verbs <- read_csv("data/input/verbs.csv") %>%
  filter(target_child_id %in% providence.kids) %>%
  mutate(age_scaled = scale(age), 
         verbs_scaled = scale(verbs))

rate <- read_csv("data/input/rate.csv") %>%
  filter(target_child_id %in% providence.kids) %>%
  mutate(age_scaled = scale(age), 
         rate_scaled = scale(rate))

input <- read_csv("data/full-input.csv") %>%
  filter(corpus_name == "Providence") %>%
  mutate(age_scaled = scale(age),
         complexity_scaled = scale(complexity), 
         rarity_scaled = scale(rarity), 
         length_scaled = scale(length))

# no effect of mean pitch on form
m.pitch.mean <- glmer(form_numeric ~ pitch_mean_scaled * age_scaled + 
                        (1|pair) + 
                        (1|speaker_id), 
                      data = pitch, 
                      family = binomial, 
                      control = glmerControl(optimizer = "bobyqa"))
summary(m.pitch.mean)

tidy(m.pitch.mean) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/mean-pitch.csv")
  
# marginal effect of pitch range on form
m.pitch.range <- glmer(form_numeric ~ pitch_range_scaled * age_scaled + 
                         (1|pair) + 
                         (1|speaker_id), 
                       data = pitch, 
                       family = binomial, 
                       control = glmerControl(optimizer = "bobyqa"))
summary(m.pitch.range)

tidy(m.pitch.range) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/pitch-range.csv")

# sig effect of lexical complexity on form
m.rate <- glmer(form_numeric ~ rate_scaled * age_scaled + 
                        (1|pair) + 
                        (1|speaker_id), 
                        data = rate, 
                      family = binomial, 
                      control = glmerControl(optimizer = "bobyqa"))
summary(m.rate)

tidy(m.rate) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/rate.csv")

# sig effect of lexical complexity on form
m.complexity <- glmer(form_numeric ~ complexity_scaled * age_scaled + 
                        (1|pair) + 
                        (1|speaker_id),
                        data = input, 
                      family = binomial, 
                      control = glmerControl(optimizer = "bobyqa"))
summary(m.complexity)

tidy(m.complexity) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/complexity.csv")

# sig effect of lexical rarity on form
m.rarity <- glmer(form_numeric ~ rarity_scaled * age_scaled + 
                        (1|pair) + 
                        (1|speaker_id),
                      data = input, 
                      family = binomial, 
                      control = glmerControl(optimizer = "bobyqa"))
summary(m.rarity)

tidy(m.rarity) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/rarity.csv")

# sig effect of utterance length on form
m.length <- glmer(form_numeric ~ length_scaled * age_scaled + 
                    (1|pair) + 
                    (1|speaker_id),
                  data = input, 
                  family = binomial, 
                  control = glmerControl(optimizer = "bobyqa"))
summary(m.length)

tidy(m.length) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/length.csv")

# sig effect of verbs on form
m.verbs <- glmer(form_numeric ~ verbs_scaled * age_scaled + 
                    (1|pair) + 
                    (1|speaker_id),
                 data = verbs, 
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
summary(m.verbs)

tidy(m.verbs) %>%
  filter(effect == "fixed") %>%
  write_csv("analysis/model-outputs/providence-input/verbs.csv")