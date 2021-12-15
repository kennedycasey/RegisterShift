library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(performance)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances <- read.csv("~/Desktop/secure/ldp_data_prepped.csv") 

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

aoa <- read_csv("data_prep/item_info.csv") %>%
  select(word, aoa, pair, form)

colors <- c("CDS" = "#C1292E", "ADS" = "#235789")

CDS_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form == "CDS") %>%
  pull(word)

ADS_forms <- read_csv("data_prep/item_info.csv") %>%
  filter(form == "ADS") %>%
  pull(word)


# CHILDES -----------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# create empty list to be populated
get_utts <- list() 
# loop over all items to get num_tokens for all utterances containing a target word
for (i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss, stem) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss, stem) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss, stem) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss, stem) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss, stem) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_id, speaker_role, num_tokens, gloss, stem) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_utts[[i]] <- utts_w_target
}

utts <- do.call(rbind, get_utts)

aoa <- read_csv("data_prep/aoa.csv")

known_words <- utts %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
  left_join(aoa, by = "word") %>%
  mutate(known = case_when(
    !is.na(aoa) & aoa <= age ~ "known",
    !is.na(aoa) & aoa > age ~ "not_known")) %>%
  filter(!is.na(known)) %>%
  group_by(id, known) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "known", values_from = "n") %>%
  group_by(id) %>%
  summarize(known_prop = known/(known + not_known))

known_props <- utts %>%
  left_join(known_words, by = c("id")) %>%
  mutate(complexity = -log(known_prop), 
         age_scaled = scale(age), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1))

m <- glmer(form_numeric ~ complexity * age_scaled + (1 + complexity * age_scaled|pair) + (1 + complexity * age_scaled|speaker_id), 
           data = known_props, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)


complexity_byword <- known_props %>%
  group_by(item) %>%
  summarize(complexity = mean(complexity, na.rm = TRUE), 
            pair = pair, 
            form = form) %>%
  distinct()

complexity_byword_summary <- known_props %>%
  filter(!is.na(complexity)) %>%
  group_by(form) %>%
  summarize(mean = mean(complexity), 
            se = sd(complexity)/sqrt(length(complexity)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_bar(data = complexity_byword_summary, 
                  aes(x = form, y = mean,
                      color = form, fill = form), 
                  stat = "identity", alpha = 0.6) +
  geom_errorbar(data = complexity_byword_summary, 
                aes(x = form, y = mean, ymin = mean - se, ymax = mean + se),
                stat = "identity", position = position_dodge(0.9),
                width = 0.1, color = "black") +
  geom_line(data = complexity_byword, aes(x = form, y = complexity, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = complexity_byword, aes(x = form, y = complexity), 
             color = "#F2F2F2", size = 2) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Lexical complexity", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
#ggsave("figs/mlu_overall.jpg", height = 5, width = 4, dpi = 300)

prod <- read_csv("data_prep/prod_norms.csv") %>%
  select(word, age, prod_prop) %>%
  group_by(word) %>%
  mutate(max_prop = max(prod_prop))

prod_words_younger <- utts %>%
  filter(age <= 30) %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
  left_join(prod, by = c("word", "age")) %>%
  filter(!is.na(prod_prop)) %>%
  group_by(id) %>%
  summarize(mean = mean(prod_prop))

prod_words_older <- utts %>%
  filter(age > 30) %>%
  mutate(word = tolower(stem)) %>%
  separate_rows(word, sep = " ") %>%
  filter(!str_detect(word, " |dog|cat|pig|stomach|mommy|daddy|mom|dad|frog|blanket|duck|rabbit|bunny|potty|bathroom|doll|horse|bird")) %>%
  left_join(prod %>% filter(age == 30) %>% select(word, max_prop), by = c("word")) %>%
  rename(prod_prop = max_prop) %>%
  filter(!is.na(prod_prop)) %>%
  group_by(id) %>%
  summarize(mean = mean(prod_prop))

prod_words <- bind_rows(prod_words_younger, prod_words_older)

prod_props <- utts %>%
  left_join(prod_words, by = c("id")) %>%
  mutate(prod_scaled = scale(mean), 
         age_scaled = scale(age), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1))

m <- glmer(form_numeric ~ prod_scaled * age_scaled + (1|pair) + (1|speaker_id), 
           data = prod_props, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)


m <- glmer(form_numeric ~ prod_scaled * age_scaled + (1 + prod_scaled * age_scaled|pair) + (1 + prod_scaled * age_scaled|speaker_id), 
           data = prod_props, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
check_collinearity(m)

coefs <- data.frame(coef(summary(m))) %>%
  rownames_to_column(var = "Predictor") %>%
  rename(SE = Std..Error, 
         z = z.value, 
         p = Pr...z..)
write_csv(coefs, "model-outputs/other-utts/prod_props.csv")


prod_byword <- prod_props %>%
  group_by(item) %>%
  summarize(complexity = mean(mean, na.rm = TRUE), 
            pair = pair, 
            form = form) %>%
  distinct()

prod_byword_summary <- prod_props %>%
  filter(!is.na(mean)) %>%
  group_by(form) %>%
  summarize(mean = mean(mean), 
            se = sd(mean)/sqrt(length(mean)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_bar(data = prod_byword_summary, 
           aes(x = form, y = mean,
               color = form, fill = form), 
           stat = "identity", alpha = 0.6) +
  geom_errorbar(data = prod_byword_summary, 
                aes(x = form, y = mean, ymin = mean - se, ymax = mean + se),
                stat = "identity", position = position_dodge(0.9),
                width = 0.1, color = "black") +
  geom_line(data = prod_byword, aes(x = form, y = complexity, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = prod_byword, aes(x = form, y = complexity), 
             color = "#F2F2F2", size = 2) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Lexical complexity", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
#ggsave("figs/mlu_overall.jpg", height = 5, width = 4, dpi = 300)




















iterations <- utterances %>%
  select(transcript_id, speaker_id) %>%
  distinct() %>%
  mutate(index = row_number())

get_order_info <- list()
for (i in unique(iterations$index)) {
  transcript_id_value <- (filter(iterations, index == i))$transcript_id
  speaker_id_value <- (filter(iterations, index == i))$speaker_id
  
  transcript_by_speaker <- filter(utterances, transcript_id == transcript_id_value & speaker_id == speaker_id_value) %>%
    mutate(utterance = row_number())
  
  get_order_info[[i]] <- transcript_by_speaker
}

ordered_utts <- do.call(rbind, get_order_info) 

preceding_utts <- ordered_utts %>%
  mutate(utterance = utterance+1, 
        preceding_gloss = case_when(
           utterance == 0 ~ "<>", 
           utterance >= 1 ~ paste(trimws(gloss))), 
        preceding_gloss = trimws(preceding_gloss)) %>%
  select(preceding_gloss, transcript_id, speaker_id, utterance)

preceding_utts2 <- ordered_utts %>%
  mutate(utterance = utterance+2, 
         preceding_gloss2 = case_when(
           utterance <= 1 ~ "<>", 
           utterance >= 2 ~ paste(trimws(gloss))), 
         preceding_gloss2 = trimws(preceding_gloss2)) %>%
  select(preceding_gloss2, transcript_id, speaker_id, utterance)

preceding_utts3 <- ordered_utts %>%
  mutate(utterance = utterance+3, 
         preceding_gloss3 = case_when(
           utterance <= 2 ~ "<>", 
           utterance >= 3 ~ paste(trimws(gloss))), 
         preceding_gloss3 = trimws(preceding_gloss3)) %>%
  select(preceding_gloss3, transcript_id, speaker_id, utterance)

following_utts <- ordered_utts %>%
  mutate(utterance = utterance-1, 
         following_gloss = case_when(
           utterance == 0 ~ "<>", 
           utterance >= 1 ~ paste(trimws(gloss))), 
         following_gloss = trimws(following_gloss)) %>%
  select(following_gloss, transcript_id, speaker_id, utterance)

following_utts2 <- ordered_utts %>%
  mutate(utterance = utterance-2, 
         following_gloss2 = case_when(
           utterance <= 1 ~ "<>", 
           utterance >= 2 ~ paste(trimws(gloss))), 
         following_gloss2 = trimws(following_gloss2)) %>%
  select(following_gloss2, transcript_id, speaker_id, utterance)

following_utts3 <- ordered_utts %>%
  mutate(utterance = utterance-3, 
         following_gloss3 = case_when(
           utterance <= 2 ~ "<>", 
           utterance >= 3 ~ paste(trimws(gloss))), 
         following_gloss3 = trimws(following_gloss3)) %>%
  select(following_gloss3, transcript_id, speaker_id, utterance)

ordered_utts_w_context <- ordered_utts %>%
  left_join(preceding_utts, by = c("transcript_id", "speaker_id", "utterance")) %>%
  left_join(following_utts, by = c("transcript_id", "speaker_id", "utterance")) %>%
  mutate(preceding_gloss = replace_na(preceding_gloss, "<>"), 
         following_gloss = replace_na(following_gloss, "<>"), 
         gloss = trimws(gloss),
         gloss_grouped = str_remove_all(paste(preceding_gloss, gloss, following_gloss), " xxx|xxx |xxx| yyy|yyy |yyy")) %>%
  filter(str_detect(gloss_grouped, "<>", negate = TRUE))

ordered_utts_w_context <- ordered_utts %>%
  mutate(gloss_grouped = str_remove_all(paste0(" ", trimws(gloss), " "), " xxx | yyy "))

merged_utts <- ordered_utts_w_context %>%
  select(id, speaker_id, utterance, gloss_grouped) %>%
  right_join(utts, by = c("id", "speaker_id")) %>%
  select(id, transcript_id, speaker_id, speaker_role, target_child_id, 
         age, item, pair, form, utterance, gloss_grouped) %>%
  filter(!is.na(gloss_grouped)) %>% # rm first/last utterances in transcript
  mutate(token_count = str_count(gloss_grouped, " ") + 1, 
         iteration = row_number())

get_types <- list()
for (i in unique(merged_utts$iteration)) {
  types <- merged_utts %>%
    filter(iteration == i) %>%
    mutate(gloss_grouped = strsplit(gloss_grouped, " ")) %>% 
    unnest(gloss_grouped) %>%
    select(gloss_grouped) %>%
    distinct() %>%
    summarize(type_count = n()) %>%
    mutate(iteration = i)
  
  get_types[[i]] <- types
}

ttr <- do.call(rbind, get_types) %>%
  right_join(merged_utts, by = "iteration") %>%
  mutate(ttr = type_count/token_count*100, 
         form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1), 
         ttr_scaled = scale(ttr), 
         age_scaled = scale(age))

#ttr$form <- factor(ttr$form, levels = c("CDS", "ADS"))


# flip analysis structure
m <- glmer(form_numeric ~ ttr_scaled * age_scaled + (1 + ttr_scaled * age_scaled|pair) + (1 + ttr_scaled * age_scaled|speaker_id), 
           data = ttr, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)

overall_trend <- ggpredict(m, c("ttr_scaled [all]"), type = "random")

ggplot() + 
  geom_smooth(data=ttr, aes(x=ttr_scaled, y=form_numeric, group=pair), method="glm", method.args=list(family = "binomial"),
              color="white", se=FALSE) +
  #geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              #fill="#235789", alpha=0.25) +
  #geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  labs(x = "TTR for +/- 1 utterance (scaled)", y = "Probability of utterance containing ADS form") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/lexical_diversity_blank.jpg")





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
  ylim(75, 90) +
  labs(x = "form", y = "TTR for +/- 1 utterance", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/lexical_diversity_overall.jpg", height = 5, width = 4, dpi = 300)

# no difference in TTR
wilcox.test(ttr ~ form, data = ttr_byword, paired = TRUE)
shapiro.test(filter(ttr_byword, form == "CDS")$ttr) #check for normality
shapiro.test(filter(ttr_byword, form == "ADS")$ttr) 

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
  labs(x = "Age (months)", y = "TTR for +/- 1 utterance", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/lexical_diversity_over_time.jpg", height = 5, width = 6, dpi = 300)

# TO DO: test expanded context window
ordered_utts_w_context_wider <- ordered_utts %>%
  left_join(preceding_utts, by = c("transcript_id", "speaker_id", "utterance")) %>%
  left_join(preceding_utts2, by = c("transcript_id", "speaker_id", "utterance")) %>%
  left_join(preceding_utts3, by = c("transcript_id", "speaker_id", "utterance")) %>%
  left_join(following_utts, by = c("transcript_id", "speaker_id", "utterance")) %>%
  left_join(following_utts2, by = c("transcript_id", "speaker_id", "utterance")) %>%
  left_join(following_utts3, by = c("transcript_id", "speaker_id", "utterance")) %>%
  mutate(preceding_gloss = replace_na(preceding_gloss, "<>"), 
         preceding_gloss2 = replace_na(preceding_gloss2, "<>"),
         preceding_gloss3 = replace_na(preceding_gloss3, "<>"), 
         following_gloss = replace_na(following_gloss, "<>"), 
         following_gloss2 = replace_na(following_gloss2, "<>"), 
         following_gloss3 = replace_na(following_gloss3, "<>"), 
         gloss = trimws(gloss),
         gloss_grouped = str_remove_all(paste(preceding_gloss3, preceding_gloss2, preceding_gloss, gloss, following_gloss, following_gloss2, following_gloss3), " xxx|xxx |xxx| yyy|yyy |yyy")) %>%
  filter(str_detect(gloss_grouped, "<>", negate = TRUE))

