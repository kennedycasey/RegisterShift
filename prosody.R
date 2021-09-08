library(tidyverse)
library(lme4)
library(lmerTest)

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

aoa <- read_csv("data_prep/item_info.csv") %>%
  select(word, aoa, pair, form)

colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

CDL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="CDL") %>%
  pull(word)

ADL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="ADL") %>%
  pull(word)

# read in pitch info from data_prep folder
# merge into single df
path_to_pitch_info = "data_prep/prosody/processed/"
pitch_filenames <- list.files(path = path_to_pitch_info, pattern = "*.csv")
pitch_files <- lapply(paste0(path_to_pitch_info, pitch_filenames), read_csv)
pitch_info <- do.call(rbind, pitch_files) %>%
  mutate(form = case_when(
      item %in% CDL_forms ~ "CDL", 
      item %in% ADL_forms ~ "ADL")) %>%
  left_join(pairs %>% rename(item = word), by = "item") %>%
  filter(across(starts_with("pitch"), ~ . != "audio file missing" &
                  . != "relevant audio clip missing" &
                  . != "utterance too short to analyze" &
                  . != "--undefined--")) %>%
  mutate(across(starts_with("pitch"), ~ as.numeric(as.character(.))))

pitch_info$form <- factor(pitch_info$form, levels = c("CDL", "ADL"))

m <- lmer(pitch_mean ~ form*age + (1|item) + (1|speaker_id), data = pitch_info)
summary(m)

mean_pitch_byword <- pitch_info %>%
  group_by(item) %>%
  summarize(pitch = mean(pitch_mean), 
            pair = pair, 
            form = form) %>%
  distinct()

mean_pitch_byword_summary <- mean_pitch_byword  %>%
  group_by(form) %>%
  summarize(mean = mean(pitch), 
            se = sd(pitch)/sqrt(length(pitch)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = mean_pitch_byword, aes(x = form, y = pitch, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = mean_pitch_byword, aes(x = form, y = pitch), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = mean_pitch_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Mean pitch (Hz)", title = "CHILDES") +
  #scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/pitch_overall.jpg", height = 5, width = 4, dpi = 300)

pitch_info %>%
  group_by(item, age) %>%
  summarize(pitch = mean(pitch_mean), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(pitch = mean(pitch)) %>%
  ggplot(aes(x = age, y = pitch, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  #scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "Mean pitch (Hz)", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/pitch_over_time.jpg", height = 5, width = 6, dpi = 300)

# compare pitch variability 
m <- lmer(abs(pitch_range) ~ form*age + (1|item) + (1|speaker_id), data = pitch_info)
summary(m)

pitch_variability_byword <- pitch_info %>%
  group_by(item) %>%
  summarize(pitch = mean(abs(pitch_range)), 
            pair = pair, 
            form = form) %>%
  distinct()

pitch_variability_byword_summary <- pitch_variability_byword  %>%
  group_by(form) %>%
  summarize(mean = mean(pitch), 
            se = sd(pitch)/sqrt(length(pitch)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = pitch_variability_byword, aes(x = form, y = pitch, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = pitch_variability_byword, aes(x = form, y = pitch), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = pitch_variability_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Pitch range (Hz)", title = "CHILDES") +
  #scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/pitch_variability_overall.jpg", height = 5, width = 4, dpi = 300)

pitch_info %>%
  group_by(item, age) %>%
  summarize(pitch = mean(abs(pitch_range)), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(pitch = mean(pitch)) %>%
  ggplot(aes(x = age, y = pitch, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  #scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "Mean pitch (Hz)", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/pitch_variability_over_time.jpg", height = 5, width = 6, dpi = 300)