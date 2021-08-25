library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)

# read in childes ldp data
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  pull(pair)

aoa <- read_csv("data_prep/item_info.csv") %>%
  select(word, aoa, pair, form)

colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

CDL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="CDL") %>%
  pull(word)

ADL_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form=="ADL") %>%
  pull(word)


# CHILDES -----------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# create empty list to be populated
get_speech_rate <- list() 

# loop over all items to get speech rate for all utterances containing a target word
for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, 
             speaker_id, media_start, media_end, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair), 
             speech_rate = num_tokens/(media_end - media_start))
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, 
             speaker_id, media_start, media_end, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair), 
             speech_rate = num_tokens/(media_end - media_start))
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, 
             speaker_id, media_start, media_end, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair), 
             speech_rate = num_tokens/(media_end - media_start))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, 
             speaker_id, media_start, media_end, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair), 
             speech_rate = num_tokens/(media_end - media_start))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, 
             speaker_id, media_start, media_end, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair), 
             speech_rate = num_tokens/(media_end - media_start))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role,
             speaker_id, media_start, media_end, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair), 
             speech_rate = num_tokens/(media_end - media_start))
  
  get_speech_rate[[i]] <- utts_w_target
  
}

speech_rate <- do.call(rbind, get_speech_rate) %>%
  filter(!is.na(speech_rate)) #51.6% of utterances not time-stamped
speech_rate$form <- factor(speech_rate$form, levels = c("CDL", "ADL"))

# singular model fit but qualitatively similar results when by-item random intercept removed
m <- lmer(speech_rate ~ form*age + (1|item) + (1|target_child_id) + (1|speaker_id), data = speech_rate)
summary(m)

speech_rate_byword <- speech_rate %>%
  group_by(item) %>%
  summarize(speech_rate = mean(speech_rate), 
            pair = pair, 
            form = form) %>%
  distinct()

speech_rate_byword_summary <- speech_rate_byword %>%
  group_by(form) %>%
  summarize(mean = mean(speech_rate), 
            se = sd(speech_rate)/sqrt(length(speech_rate)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = speech_rate_byword, aes(x = form, y = speech_rate, group = pair), 
            color = "#F5F5F5", size = 1) +
  geom_point(data = speech_rate_byword, aes(x = form, y = speech_rate), 
             color = "#F5F5F5", size = 2) +
  geom_pointrange(data = speech_rate_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "form", y = "utterance-level speech rate (tokens/s)", title = "CHILDES") +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/speech_rate_overall.jpg", height = 5, width = 4, dpi = 300)

# no difference in speech rate
wilcox.test(speech_rate ~ form, data = speech_rate_byword, paired = TRUE)
shapiro.test(filter(speech_rate_byword, form == "CDL")$speech_rate) #check for normality -> t-test not valid
shapiro.test(filter(speech_rate_byword, form == "ADL")$speech_rate)
