library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances <- read.csv("~/Desktop/secure/ldp_data_prepped.csv") 

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


# CHILDES -----------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(target_child_age < 84, speaker_role != "Target_Child") %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

get_ttr <- list()

for (i in unique(utterances$transcript_id)){
  tokens <- utterances %>%
    filter(transcript_id == i) %>%
    summarize(token_count = sum(num_tokens))
  
  types <- utterances %>%
    filter(transcript_id == i) %>%
    mutate(stem = strsplit(tolower(stem), " ")) %>% 
    unnest(stem) %>%
    select(stem) %>%
    distinct() %>%
    summarize(type_count = n())
  
  ttr <- cbind(tokens, types) %>%
    mutate(ttr = type_count/token_count*100, 
           transcript_id = paste(i))
  
  get_ttr[[i]] <- ttr
}

# TO DO: spot check stem counts for subset of transcripts
transcript_ttr <- do.call(rbind, get_ttr) %>%
  filter(ttr > 0) %>%
  mutate(transcript_id = as.numeric(as.character(transcript_id)))

# create empty list to be populated
get_utts <- list() 
# loop over all items to get num_tokens for all utterances containing a target word
for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens, gloss) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_utts[[i]] <- utts_w_target
  
}

utts <- do.call(rbind, get_utts)

ttr <- utts %>%
  left_join(transcript_ttr, by = "transcript_id") %>%
  select(target_child_id, transcript_id, age, item, form, pair, ttr) %>%
  distinct()

ttr$form <- factor(ttr$form, levels = c("CDL", "ADL"))

m <- lmer(ttr ~ form*age + (1|item) + (1|target_child_id), data = ttr)
summary(m)

ttr_byword <- ttr %>%
  group_by(item) %>%
  summarize(ttr = mean(ttr, na.rm = TRUE), 
            pair = pair, 
            form = form) %>%
  distinct()

ttr_byword_summary <- ttr_byword %>%
  group_by(form) %>%
  summarize(mean = mean(ttr, na.rm = TRUE), 
            se = sd(ttr, na.rm = TRUE)/sqrt(length(ttr)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = ttr_byword, aes(x = form, y = ttr, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = ttr_byword, aes(x = form, y = ttr), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = ttr_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Transcript-level type:token ratio", title = "CHILDES") +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/ttr_overall.jpg", height = 5, width = 4, dpi = 300)

# no difference in TTR
t.test(ttr ~ form, data = ttr_byword, paired = TRUE)
shapiro.test(filter(ttr_byword, form == "CDL")$ttr) #check for normality
shapiro.test(filter(ttr_byword, form == "ADL")$ttr) 

ttr %>%
  group_by(item, age) %>%
  summarize(ttr = mean(ttr, na.rm = TRUE), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(ttr = mean(ttr, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = ttr, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Transcript-level type:token ratio", title = "CHILDES") +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/ttr_over_time.jpg", height = 5, width = 6, dpi = 300)