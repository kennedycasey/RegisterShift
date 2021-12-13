library(tidyverse)
library(lme4)
library(lmerTest)

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

colors <- c("CDS" = "#C1292E", "ADS" = "#235789")

CDS_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="CDS") %>%
  pull(word)

ADS_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="ADS") %>%
  pull(word)

# read in pitch info from data_prep folder
# merge into single df
path_to_pitch_info = "data_prep/prosody/processed/"
pitch_filenames <- list.files(path = path_to_pitch_info, pattern = "*.csv")
pitch_files <- lapply(paste0(path_to_pitch_info, pitch_filenames), read_csv)
pitch_info <- do.call(rbind, pitch_files) %>%
  mutate(form = case_when(
      item %in% CDS_forms ~ "CDS", 
      item %in% ADS_forms ~ "ADS")) %>%
  left_join(pairs %>% rename(item = word), by = "item") %>%
  filter(across(starts_with("pitch"), ~ . != "audio file missing" &
                  . != "relevant audio clip missing" &
                  . != "utterance too short to analyze" &
                  . != "--undefined--")) %>%
  mutate(across(starts_with("pitch"), ~ as.numeric(as.character(.)))) %>%
  group_by(item) %>%
  filter(media_end - media_start > 0.5) %>%
  mutate(form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1), 
         pitch_range = abs(pitch_range),
         pitch_mean_scaled = scale(pitch_mean), 
         pitch_range_scaled = scale(pitch_range),
         age_scaled = scale(age))

write.csv(data.frame(pitch_info), "prosody.csv", row.names = FALSE)

#flip analysis structure
m <- glmer(form_numeric ~ pitch_mean_scaled * age_scaled + (1+pitch_mean_scaled*age_scaled|pair) + (1+pitch_mean_scaled*age_scaled|speaker_id), 
           data = pitch_info, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)


test <- pitch_info %>%
  mutate(form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1)) %>%
  group_by(speaker_id, pair, form) %>%
  summarize(mean = mean(pitch_mean), 
            range = mean(abs(pitch_range)), 
            n = n()) %>%
  pivot_wider(names_from = form, values_from = c(mean:n)) %>%
  na.omit() %>%
  mutate(diff_mean = mean_CDS - mean_ADS, 
         diff_range = range_CDS - range_ADS)

test2 <- filter(test, pair == "daddy_dad")
t.test(test2$diff_mean, mu = 0)
t.test(test2$diff_range, mu = 0)

m <- lmer()
            
m <- glmer(form_numeric ~ pitch_mean_scaled * age_scaled + (1|pair) + (1|speaker_id), 
           data = pitch_info, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)

overall_trend <- ggpredict(m, c("pitch_mean_scaled [all]"), type = "random")

ggplot() + 
  geom_smooth(data=pitch_info, aes(x=pitch_mean_scaled, y=form_numeric, group=pair), method="glm", method.args=list(family = "binomial"),
              color="#F5F5F5", se=FALSE) +
  geom_ribbon(data=overall_trend, aes(x=x+2.25, ymin=predicted-conf.low, ymax=predicted+conf.low), 
  fill="#235789", alpha=0.25) +
  geom_line(data=overall_trend, aes(x=x+2.25, y=predicted), color="#235789", size = 2) +
  labs(x = "Utterance-level mean pitch (Hz, scaled)", y = "Probability of utterance containing ADS form") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/pitch.jpg")










#pitch_info$form <- factor(pitch_info$form, levels = c("CDS", "ADS"))

m <- lmer(pitch_mean ~ form*age + (1|pair) + (1|speaker_id), data = pitch_info)
summary(m)

library(gghalves)
 
ggplot(pitch_info, aes(x = pair, y = pitch_mean, color = form, fill = form)) +
  geom_jitter(alpha = 0.1) +
  geom_half_violin(data = (filter(pitch_info, form == "ADS")), aes(x = pair, y = pitch_mean, color = form, fill = form), 
                       stat = "half_ydensity", side = "r", alpha = 0.5) +
  geom_half_violin(data = (filter(pitch_info, form == "CDS")), aes(x = pair, y = pitch_mean, color = form, fill = form), 
                       stat = "half_ydensity", side = "l", alpha = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "form", y = "Mean pitch (Hz)", title = "CHILDES") +
  #scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

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
  labs(x = "form", y = "Mean pitch (Hz)", title = "CHILDES") +
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
  labs(x = "form", y = "Pitch range (Hz)", title = "CHILDES") +
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