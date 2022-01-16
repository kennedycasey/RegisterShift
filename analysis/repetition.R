library(childesr)
library(wordbankr)
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

source("data-prep/overall/functions.R")
get_utts_w_target(childes_utterances, items)

function_words <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(lexical_category == "function_words") %>%
  mutate(word = str_remove_all(uni_lemma, "(\\s*\\(\\w+\\))")) %>%
  pull(word)

`%notin%` <- Negate(`%in%`)
repeated_words <- utterances %>%
  filter(speaker_type == "other") %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(word %notin% function_words) %>%
  group_by(id) %>%
  summarize(n = n(), 
            n_unique = length(unique(word))) %>%
  filter(n != 1) %>%
  mutate(repeated_prop = (n - n_unique)/n) %>%
  select(id, repeated_prop)

repeated_props <- utterances %>%
  filter(speaker_type == "other") %>%
  left_join(repeated_words, by = c("id")) %>%
  mutate(repetition_scaled = scale(repeated_prop),
         age_scaled = scale(age))

m <- glmer(form_numeric ~ repetition_scaled * age_scaled + 
             (1 + repetition_scaled + age_scaled|pair) + 
             (1 + repetition_scaled + age_scaled|speaker_id), 
           data = repeated_props, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/repitition.csv")

mlu_byword <- mlu_other %>%
  group_by(item) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            form = form) %>%
  distinct()

mlu_byword_summary <- mlu_byword %>%
  group_by(form) %>%
  summarize(mean = mean(mlu), 
            se = sd(mlu)/sqrt(length(mlu)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = mlu_byword, aes(x = form, y = mlu, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = mlu_byword, aes(x = form, y = mlu), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = mlu_byword_summary, 
                  aes(x = form, y = mean, 
                      ymin = mean - se, ymax = mean + se, 
                      color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "MLUw") +
  scale_y_continuous(limits = c(3, 9), breaks = seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("writing/figs/input/MLU.jpg", height = 5, width = 4, dpi = 300)

m <- glmer(form_numeric ~ MLUm_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_other, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/MLUm.csv")

# predict form from MLU: child-produced utts ------------------------------
m <- glmer(form_numeric ~ MLUw_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_child, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/child-utts/MLUw.csv")

m <- glmer(form_numeric ~ MLUm_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_child, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/child-utts/MLUm.csv")

