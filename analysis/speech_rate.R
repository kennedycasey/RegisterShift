library(childesr)
library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)

# get timestamped utterances containing target word
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

source("data-prep/overall/functions.R")
source("data-prep/overall/get_min_dur.R")
get_utts_w_target(childes_utterances, items)

timestamped_other <- utterances %>%
  filter(speaker_type == "other" & !is.na(media_start) & 
           !is.na(media_end) & (media_end - media_start) >= min_dur/1000) %>%
  mutate(speech_rate = num_tokens/(media_end - media_start), 
         speech_rate_scaled = scale(speech_rate), 
         age_scaled = scale(age))

timestamped_child <- utterances %>%
  filter(speaker_type == "child" & !is.na(media_start) & 
           !is.na(media_end) & (media_end - media_start) >= min_dur/1000) %>%
  mutate(speech_rate = num_tokens/(media_end - media_start), 
         speech_rate_scaled = scale(speech_rate), 
         age_scaled = scale(age))

m <- glmer(form_numeric ~ speech_rate_scaled * age_scaled + 
             (1|pair) + (1|speaker_id), 
           data = timestamped_other, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/speech-rate.csv")

rate_byword <- timestamped_other %>%
  group_by(item) %>%
  summarize(mean_rate = mean(speech_rate), 
            pair = pair, 
            form = form) %>%
  distinct()

rate_byword_summary <- rate_byword %>%
  group_by(form) %>%
  summarize(mean = mean(mean_rate), 
            se = sd(mean_rate)/sqrt(length(mean_rate)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = rate_byword, aes(x = form, y = mean_rate, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = rate_byword, aes(x = form, y = mean_rate), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = rate_byword_summary, 
                  aes(x = form, y = mean, 
                      ymin = mean - se, ymax = mean + se, 
                      color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Mean Speech Rate (words/s)") +
  scale_y_continuous(limits = c(1.5, 3.5), breaks = seq(1.5, 3.5, by = 1)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("writing/figs/input/speech-rate.jpg", height = 5, width = 4, dpi = 300)