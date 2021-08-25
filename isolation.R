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
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# create empty list to be populated
get_mlu <- list() 
# loop over all items to get num_tokens for all utterances containing a target word
for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
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
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
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
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
}

mlu <- do.call(rbind, get_mlu)
mlu$form <- factor(mlu$form, levels = c("CDL", "ADL"))

mlu <- mlu %>%
  mutate(isolated = case_when(
    num_tokens == 1 ~ 1,
    num_tokens > 1 ~ 0
  ))

# main effect of form
# isolation decreases with age
# negative interaction between form and age
m <- glmer(isolated ~ form*age + (1|item) + (1|target_child_id),
          family = binomial, 
          data = mlu)
summary(m)

isolated_byword <- mlu %>%
  group_by(item) %>%
  summarize(isolated = mean(isolated), 
            pair = pair, 
            form = form) %>%
  distinct()

isolated_byword_summary <- isolated_byword %>%
  group_by(form) %>%
  summarize(mean = mean(isolated), 
            se = sd(isolated)/sqrt(length(isolated)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = isolated_byword, aes(x = form, y = isolated, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = isolated_byword, aes(x = form, y = isolated), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = isolated_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Rate of isolated utterances", title = "CHILDES") +
  ylim(0, 0.15) +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/isolation_overall.jpg", height = 5, width = 4, dpi = 300)

# no difference in isolation
t.test(isolated ~ form, data = isolated_byword, paired = TRUE)
shapiro.test(filter(isolated_byword, form == "CDL")$isolated) #check for normality
shapiro.test(filter(isolated_byword, form == "ADL")$isolated) 

mlu %>%
  group_by(item, age) %>%
  summarize(isolated = mean(isolated), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(isolated = mean(isolated)) %>%
  ggplot(aes(x = age, y = isolated, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  #scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "Rate of isolated utterances", title = "CHILDES") +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/isolation_over_time.jpg", height = 5, width = 6, dpi = 300)

# Providence --------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(corpus_name == "Providence" & speaker_role != "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# create empty list to be populated
get_mlu <- list() 
# loop over all items to get num_tokens for all utterances containing a target word
for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                            root, "eys | ", root, "ies | ",
                                            root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
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
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
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
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
}

mlu <- do.call(rbind, get_mlu)
mlu$form <- factor(mlu$form, levels = c("CDL", "ADL"))

mlu <- mlu %>%
  mutate(isolated = case_when(
    num_tokens == 1 ~ 1,
    num_tokens > 1 ~ 0
  ))

# more isolation for CDL
# isolation decreases with age
# positive interaction between form and age
m <- glmer(isolated ~ form*age + (1|item) + (1|target_child_id),
           family = binomial, 
           data = mlu)
summary(m)

isolated_byword <- mlu %>%
  group_by(item) %>%
  summarize(isolated = mean(isolated), 
            pair = pair, 
            form = form) %>%
  distinct()

isolated_byword_summary <- isolated_byword %>%
  group_by(form) %>%
  summarize(mean = mean(isolated), 
            se = sd(isolated)/sqrt(length(isolated)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = isolated_byword, aes(x = form, y = isolated, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = isolated_byword, aes(x = form, y = isolated), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = isolated_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Rate of isolated utterances", title = "Providence") +
  ylim(0, 0.15) +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/Providence/isolation_overall.jpg", height = 5, width = 4, dpi = 300)

# no difference in isolation
t.test(isolated ~ form, data = isolated_byword, paired = TRUE)
shapiro.test(filter(isolated_byword, form == "CDL")$isolated) #check for normality
shapiro.test(filter(isolated_byword, form == "ADL")$isolated) 

mlu %>%
  group_by(item, age) %>%
  summarize(isolated = mean(isolated), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(isolated = mean(isolated)) %>%
  ggplot(aes(x = age, y = isolated, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Rate of isolated utterances", title = "Providence") +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/Providence/isolation_over_time.jpg", height = 5, width = 6, dpi = 300)


# LDP ---------------------------------------------------------------------

utterances <- data.table(filter(ldp_utterances, speaker != "target_child"))

# create empty list to be populated
get_mlu <- list() 

# loop over all items to get num_tokens for all utterances containing a target word
for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                            root, "eys | ", root, "ies | ",
                                            root, "ey's | ", root, "ie's ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
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
      select(subject, session, line, age, speaker, num_tokens) %>%
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
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(subject, session, line, age, speaker, num_tokens)  %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDL_forms ~ "CDL", 
               i %in% ADL_forms ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
  
}

mlu <- do.call(rbind, get_mlu)
mlu$form <- factor(mlu$form, levels = c("CDL", "ADL"))

mlu <- mlu %>%
  mutate(isolated = case_when(
    num_tokens == 1 ~ 1,
    num_tokens > 1 ~ 0
  ))

# no main effect of form
# isolation decreases with age
# no interaction between form and age
m <- glmer(isolated ~ form*age + (1|item) + (1|subject),
           family = binomial, 
           data = mlu)
summary(m)

isolated_byword <- mlu %>%
  group_by(item) %>%
  summarize(isolated = mean(isolated), 
            pair = pair, 
            form = form) %>%
  distinct()

isolated_byword_summary <- isolated_byword %>%
  group_by(form) %>%
  summarize(mean = mean(isolated), 
            se = sd(isolated)/sqrt(length(isolated)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = isolated_byword, aes(x = form, y = isolated, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = isolated_byword, aes(x = form, y = isolated), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = isolated_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Rate of isolated utterances", title = "LDP") +
  ylim(0, 0.15) +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/LDP/isolation_overall.jpg", height = 5, width = 4, dpi = 300)

# no difference in isolation
t.test(isolated ~ form, data = isolated_byword, paired = TRUE)
shapiro.test(filter(isolated_byword, form == "CDL")$isolated) #check for normality
shapiro.test(filter(isolated_byword, form == "ADL")$isolated) 

mlu %>%
  group_by(item, age) %>%
  summarize(isolated = mean(isolated), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(isolated = mean(isolated)) %>%
  ggplot(aes(x = age, y = isolated, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Rate of isolated utterances", title = "LDP") +
  theme_test(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/LDP/isolation_over_time.jpg", height = 5, width = 6, dpi = 300)