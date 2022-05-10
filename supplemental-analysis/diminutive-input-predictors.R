library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)

utterances <- read_csv("data/childes-input.csv")

diminutives <- read_csv("data-prep/overall/item-info.csv") %>%
  filter(diminutive == "y") %>%
  pull(word)

not_diminutives <- read_csv("data-prep/overall/item-info.csv") %>%
  filter(diminutive == "n") %>%
  pull(word)

input_predictors <- c("pitch_mean", "pitch_range", "rate", "rarity", 
                      "complexity_wordbank", "complexity_ratings",
                      "verbs", "num_tokens")

path <- "data/input/"
for (i in c("child", "other")) {
  for (type in c("diminutives", "not_diminutives")) {
    data <- utterances %>%
      filter(speaker_type == i & item %in% eval(as.symbol(type))))
    
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
        mutate(term = str_remove_all(term, "scale|eval|as.symbol|[()]"), 
               term = str_replace_all(term, "j", j)) %>%
        write_csv(paste0("supplemental-analysis/model-outputs/input-predictors/", 
                         str_replace(j, "_", "-"), "-", i, "-", type, ".csv"))
  }
}
}
