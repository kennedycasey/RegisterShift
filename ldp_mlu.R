library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)

# read in ldp data from hard-coded local path
ldp_utterances <- read.csv("~/Desktop/secure/ldp_data_prepped.csv") 

utterances <- data.table(ldp_utterances)

items <- read_csv("item_info/candidate_items_new.csv") %>%
  pull(word)

ids_forms <- read_csv("item_info/candidate_items_new.csv") %>%
  filter(form == "ids") %>%
  pull(word)

ads_forms <- read_csv("item_info/candidate_items_new.csv") %>%
  filter(form == "ads") %>%
  pull(word)


pairs <- read_csv("item_info/candidate_items_new.csv") %>%
  select(word, pair)

colors <- c("ids" = "#C1292E", "ads" = "#235789")

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
               i %in% ids_forms ~ "ids", 
               i %in% ads_forms ~ "ads"), 
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
               i %in% ids_forms ~ "ids", 
               i %in% ads_forms ~ "ads"),
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
               i %in% ids_forms ~ "ids", 
               i %in% ads_forms ~ "ads"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% ids_forms ~ "ids", 
               i %in% ads_forms ~ "ads"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% ids_forms ~ "ids", 
               i %in% ads_forms ~ "ads"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(subject, session, line, age, speaker, num_tokens)  %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% ids_forms ~ "ids", 
               i %in% ads_forms ~ "ads"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
  
}

mlu <- do.call(rbind, get_mlu)
mlu$form <- factor(mlu$form, levels = c("ids", "ads"))

mlu %>%
  group_by(item, age) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ggplot(aes(x = age, y = mlu, color = form, fill = form)) +
  facet_wrap(.~pair, nrow = 5) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "age (months)", y = "MLUw") +
  theme_test(base_size = 15)
ggsave("plots/ldp/mlu/mlu_over_time.jpg", height = 15, width = 12, dpi = 300)


m <- lmer(num_tokens ~ form*age + (1|item) + (1|subject), data = mlu)
summary(m)

mlu_byword <- mlu %>%
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
  geom_bar(data = mlu_byword_summary, aes(x = form, y = mean, color = form, fill = form), 
           stat = "identity", width = 0.7, alpha = 0.5, size = 1.5) +
  geom_line(data = mlu_byword, aes(x = form, y = mlu, group = pair)) +
  geom_point(data = mlu_byword, aes(x = form, y = mlu, color = form, fill = form)) +
  geom_errorbar(data = mlu_byword_summary, aes(x = form, ymin = mean-se, ymax = mean+se), 
                stat = "identity", width = 0.15, size = 0.75, position = position_dodge(0.9)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "form", y = "MLUw") +
  theme_test(base_size = 20) +
  theme(legend.position = "none")

ggplot() +
  geom_line(data = mlu_byword, aes(x = form, y = mlu, group = pair), 
            color = "#D3D3D3", size = 1) +
  geom_point(data = mlu_byword, aes(x = form, y = mlu), 
             color = "#D3D3D3", size = 2) +
  geom_pointrange(data = mlu_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  ylim(3, 8) +
  labs(x = "form", y = "MLUw") +
  theme_test(base_size = 20) +
  theme(legend.position = "none")
ggsave("plots/ldp/mlu/mlu_overall.jpg", height = 5, width = 4, dpi = 300)

# marginal difference between ids and ads mlu
t.test(mlu ~ form, data = mlu_byword, paired = TRUE)
shapiro.test(filter(mlu_byword, form == "ids")$mlu) #check for normality
shapiro.test(filter(mlu_byword, form == "ads")$mlu) 


mlu %>%
  group_by(item, age) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            form = form) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, form) %>%
  summarize(mlu = mean(mlu)) %>%
  ggplot(aes(x = age, y = mlu, color = form, fill = form)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "age (months)", y = "MLUw") +
  theme_test(base_size = 15) +
  theme(legend.position = "none")
