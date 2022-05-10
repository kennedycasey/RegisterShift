library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)

utterances <- read_csv("data/childes-input.csv")

input_predictors <- c("pitch_mean", "pitch_range", "rate", "rarity", 
                      "complexity_wordbank", "complexity_ratings",
                      "verbs", "num_tokens")

ages <- c(12, 24, 36, 48, 60, 72, 84)


colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

path <- "data/input/"
i = "other"
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
    left_join(verbs) %>%
    mutate(age_range = case_when(
      age < 12 ~ 12, 
      age >= 12 & age < 24 ~ 24, 
      age >= 24 & age < 36 ~ 36, 
      age >= 36 & age < 48 ~ 48, 
      age >= 48 & age < 60 ~ 60, 
      age >= 60 & age < 72 ~ 72, 
      age >= 72  ~ 84)) %>%
    ungroup()
  
  for (k in ages) {
    
    age_data <- data %>%
      filter(age_range == k)
    
  for (j in input_predictors) {
    age_predictor_data <- filter(age_data, !is.na(eval(as.symbol(j))))
    
    if (nrow(age_predictor_data) < 10) {
      break
    }
    model <- glm(form_numeric ~ scale(eval(as.symbol(j))) * scale(age),
                   data = age_predictor_data, 
                   family = "binomial")
    summary(model)
    
    tidy(model) %>%
      mutate(term = str_remove_all(term, "scale|eval|as.symbol|[()]"), 
             term = str_replace_all(term, "j", j)) %>%
      write_csv(paste0("supplemental-analysis/model-outputs/input-predictors-over-age/", 
                       str_replace(j, "_", "-"), "-", i, "-", k, ".csv"))
    }
  }
}


rarity <- read_csv(paste0(path, "rarity.csv")) %>%
  select(id, item, rarity)

complexity_wordbank <- read_csv(paste0(path, "complexity-wordbank.csv")) %>%
  rename(complexity_wordbank = complexity) %>%
  select(id, item, complexity_wordbank)

complexity_ratings <- read_csv(paste0(path, "complexity-ratings.csv")) %>%
  rename(complexity_ratings = complexity) %>%
  select(id, item, complexity_ratings)

rate <- read_csv(paste0(path, "rate.csv")) %>%
  select(id, item, rate)

pitch <- read_csv(paste0(path, "pitch.csv")) %>%
  select(id, item, pitch_mean, pitch_range)

verbs <- read_csv(paste0(path, "verbs.csv")) %>%
  select(id, item, verbs)

data <- utterances %>%
  filter(speaker_type == "other") %>%
  left_join(rarity) %>%
  #left_join(complexity_wordbank) %>%
  left_join(complexity_ratings) %>%
  left_join(rate) %>%
  left_join(pitch) %>%
  left_join(verbs)

combined <- data %>%
  pivot_longer(c(rarity:verbs), 
               names_to = "predictor", 
               values_to = "value")

for (pair in unique(combined$pair)) {
  
  summary <- combined %>%
    filter(pair == pair) %>%
    group_by(age, predictor, form) %>%
    summarize(mean = mean(value, na.rm = TRUE), 
              count = n()) %>%
    distinct()
  
  ggplot(filter(combined, pair == pair), 
         aes(age, value, color = form, fill = form)) +
  facet_wrap(.~predictor, scale = "free_y") +
  geom_point(summary, 
             mapping = aes(age, mean, 
                           fill = form, 
                           color = form,
                           size = count), 
             alpha = 0.1) + 
  geom_smooth() +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) + 
  labs(x = "Age (months)", y = "Predictor Value", 
       color = "Form", fill = "Form", size = "N Utterances") + 
  theme_test(base_size = 10)
  
  ggsave(paste0("supplemental-analysis/figs/", pair, "-over-age.jpg"), 
         dpi = 300, width = 7, height = 5)
}
  
  

