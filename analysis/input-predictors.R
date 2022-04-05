library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)

utterances <- read_csv("data/childes-input.csv")

input_predictors <- c("pitch_mean", "pitch_range", "rate", "rarity", 
                      "complexity_wordbank", "complexity_ratings",
                      "verbs", "num_tokens")

path <- "data/input/"
for (i in c("child", "other")) {
  data <- utterances %>%
    filter(speaker_type == i)
  
  rarity <- read_csv(paste0(path, ifelse(i == "child", "rarity-child", 
                                         "rarity"), ".csv")) %>%
    select(id, item, rarity)
  
  complexity_wordbank <- read_csv(paste0(path, ifelse(i == "child", "complexity-wordbank-child", 
                                         "complexity-wordbank"), ".csv")) %>%
    rename(complexity_wordbank = complexity) %>%
    select(id, item, complexity_wordbank)
  
  complexity_ratings <- read_csv(paste0(path, ifelse(i == "child", "complexity-ratings-child", 
                                                      "complexity-ratings"), ".csv")) %>%
    rename(complexity_ratings = complexity) %>%
    select(id, item, complexity_ratings)
  
  rate <- read_csv(paste0(path, ifelse(i == "child", "rate-child", 
                                         "rate"), ".csv")) %>%
    select(id, item, rate)
  
  pitch <- read_csv(paste0(path, ifelse(i == "child", "pitch-child", 
                                       "pitch"), ".csv")) %>%
    select(id, item, pitch_mean, pitch_range)
  
  verbs <- read_csv(paste0(path, ifelse(i == "child", "verbs-child", 
                                       "verbs"), ".csv")) %>%
    select(id, item, verbs)
  
  data <- data %>%
    left_join(rarity) %>%
    left_join(complexity_wordbank) %>%
    left_join(complexity_ratings) %>%
    left_join(rate) %>%
    left_join(pitch) %>%
    left_join(verbs)
  
  write_csv(data, paste0("data/input/combined-", i, ".csv"))
  
  for (j in input_predictors) {
    model <- glmer(form_numeric ~ scale(eval(as.symbol(j))) * scale(age) + 
                     (1|pair) + 
                     (1|speaker_id), 
                   data = filter(data, !is.na(eval(as.symbol(j)))), 
                   family = binomial, 
                   control = glmerControl(optimizer = "bobyqa"))
    summary(model)
    
    tidy(model) %>%
      filter(effect == "fixed") %>%
      write_csv(paste0("analysis/model-outputs/input-predictors/", 
                       str_replace(j, "_", "-"), "-", i, ".csv"))
  }
  
  m.combined <- glmer(form_numeric ~ scale(complexity_wordbank) * scale(age) +
                        scale(rarity) * scale(age) +
                        scale(rate) * scale(age) +
                        scale(pitch_mean) * scale(age) +
                        scale(pitch_range) * scale(age) +
                        scale(verbs) * scale(age) +
                        scale(num_tokens) * scale(age) +
                        (1|pair) +
                        (1|speaker_id),
                      data = data,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
  summary(m.combined)

  tidy(m.combined) %>%
    filter(effect == "fixed") %>%
    write_csv(paste0("analysis/model-outputs/all-predictors-", i, ".csv"))
}