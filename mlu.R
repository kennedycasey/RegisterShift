library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(ggeffects)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances <- read.csv("~/Desktop/secure/ldp_data_prepped.csv")

length(unique(childes_utterances$transcript_id))

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  select(word, pair)

aoa <- read_csv("data_prep/item_info.csv") %>%
  select(word, aoa, pair, variant)

colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

CDL_variants <- read_csv("data_prep/item_info.csv") %>%
  filter(variant=="CDL") %>%
  pull(word)

ADL_variants <- read_csv("data_prep/item_info.csv") %>%
  filter(variant=="ADL") %>%
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
}

mlu <- do.call(rbind, get_mlu) %>%
  mutate(variant = factor(variant, levels = c("CDL", "ADL")), 
         variant_numeric = case_when(
           variant == "CDL" ~ 0, 
           variant == "ADL" ~ 1), 
         age_scaled = scale(age), 
         mlu_scaled = scale(num_tokens))


# flip analysis structure
m <- glmer(variant_numeric ~ mlu_scaled * age_scaled + (1|pair) + (1|target_child_id), 
           data = mlu, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)

library(sjPlot)
tab_model(m)

overall_trend <- ggpredict(m, c("mlu_scaled [all]"), type = "random")

ggplot() + 
  geom_smooth(data=mlu, aes(x=mlu_scaled, y=variant_numeric, group=pair), method="glm", method.args=list(family = "binomial"),
              color="#F5F5F5", se=FALSE) +
  #geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              #fill="#235789", alpha=0.25) +
  #geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  labs(x = "Number of words in utterance (scaled)", y = "Probability of utterance containing ADL variant") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/num_tokens_blank.jpg")

tidy(
  m,
  effects = c("ran_pars", "fixed"),
  scales = NULL,
  exponentiate = FALSE,
  ran_prefix = NULL,
  conf.int = FALSE,
  conf.level = 0.95,
  conf.method = "Wald",
  ddf.method = NULL,
  profile = NULL,
  debug = FALSE,
)



fe <- broom::augment(m)
plot_model(m, pred.type = "fe")
sjp.glmer(m, type = "re.qq")
sjp.glmer(m, type = "fe.pc")

effects <- effects::effect(term = "mlu_scaled", mod = m)


overall_trend <- ggpredict(m, c("(Itercept)", 
                                "mlu_scaled [all]", 
                                "age_scaled [all]", 
                                "mlu_scld:g_s"), 
                           type = "re")

ggplot() + 
  #geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric, group=pair), method="glm", method.args=list(family = "binomial"), 
  #color="#F5F5F5", se=FALSE) +
  geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  #scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADL variant", 
       title = "CHILDES") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/ADL_over_time_no_items.jpg")



m <- glmer(variant_numeric ~ mlu_scaled * age_scaled + (1|target_child_id), 
           data = filter(mlu, pair == "tummy_stomach"), 
           family = binomial, 
           control=glmerControl(optimizer="bobyqa"))
summary(m)


ggplot(mlu, aes(x = pair, y = num_tokens, color = variant, fill = variant)) +
  geom_jitter(alpha = 0.1) +
  geom_half_violin(data = (filter(mlu, variant == "ADL")), aes(x = pair, y = num_tokens, color = variant, fill = variant), 
                   stat = "half_ydensity", side = "r", alpha = 0.5) +
  geom_half_violin(data = (filter(mlu, variant == "CDL")), aes(x = pair, y = num_tokens, color = variant, fill = variant), 
                   stat = "half_ydensity", side = "l", alpha = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Item-Pair", y = "Number of Tokens Per Utterance", title = "CHILDES") +
  #scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggplot(mlu, aes(x = age, y = num_tokens, color = variant, fill = variant)) +
  facet_wrap(.~pair, nrow = 3) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Age (months)", y = "Number of Tokens Per Utterance", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

mlu_by_session <- mlu %>%
  group_by(target_child_id, transcript_id, item) %>%
  summarize(mlu = mean(num_tokens), 
            variant = variant, 
            pair = pair, 
            age = age) %>%
  distinct()

ggplot(filter(mlu_by_session, mlu < 30), aes(x = age, y = mlu, color = variant, fill = variant)) +
  facet_wrap(.~pair, nrow = 3) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Age (months)", y = "MLUw", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


# higher MLUw for ADL variants
# increase in MLUw across time
# negative interaction with time
m <- lmer(num_tokens ~ variant*age + (1|item) + (1|target_child_id), data = mlu)
summary(m)

mlu_byword <- mlu %>%
  group_by(item) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            variant = variant) %>%
  distinct()

mlu_byword_summary <- mlu_byword %>%
  group_by(variant) %>%
  summarize(mean = mean(mlu), 
            se = sd(mlu)/sqrt(length(mlu)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = mlu_byword, aes(x = variant, y = mlu, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = mlu_byword, aes(x = variant, y = mlu), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = mlu_byword_summary, aes(x = variant, y = mean, ymin = mean-se, ymax = mean+se, color = variant, fill = variant), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "variant", y = "MLUw", title = "CHILDES") +
  scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/mlu_overall.jpg", height = 5, width = 4, dpi = 300)

# higher MLUw for ADL variants
t.test(mlu ~ variant, data = mlu_byword, paired = TRUE)
shapiro.test(filter(mlu_byword, variant == "CDL")$mlu) #check for normality
shapiro.test(filter(mlu_byword, variant == "ADL")$mlu) 

mlu %>%
  group_by(item, age) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            variant = variant) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, variant) %>%
  summarize(mlu = mean(mlu)) %>%
  ggplot(aes(x = age, y = mlu, color = variant, fill = variant)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "MLUw", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/mlu_over_time.jpg", height = 5, width = 6, dpi = 300)

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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
}

mlu <- do.call(rbind, get_mlu)
mlu$variant <- factor(mlu$variant, levels = c("CDL", "ADL"))

# MLUw NOT higher for ADL variants
# increase in MLUw across time
# null interaction with time
m <- lmer(num_tokens ~ variant*age + (1|item) + (1|target_child_id), data = mlu)
summary(m)

mlu_byword <- mlu %>%
  group_by(item) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            variant = variant) %>%
  distinct()

mlu_byword_summary <- mlu_byword %>%
  group_by(variant) %>%
  summarize(mean = mean(mlu), 
            se = sd(mlu)/sqrt(length(mlu)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = mlu_byword, aes(x = variant, y = mlu, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = mlu_byword, aes(x = variant, y = mlu), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = mlu_byword_summary, aes(x = variant, y = mean, ymin = mean-se, ymax = mean+se, color = variant, fill = variant), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "variant", y = "MLUw", title = "Providence") +
  scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/Providence/mlu_overall.jpg", height = 5, width = 4, dpi = 300)

# higher MLUw for ADL variants
t.test(mlu ~ variant, data = mlu_byword, paired = TRUE)
shapiro.test(filter(mlu_byword, variant == "CDL")$mlu) #check for normality
shapiro.test(filter(mlu_byword, variant == "ADL")$mlu) 

mlu %>%
  group_by(item, age) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            variant = variant) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, variant) %>%
  summarize(mlu = mean(mlu)) %>%
  ggplot(aes(x = age, y = mlu, color = variant, fill = variant)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "MLUw", title = "Providence") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/Providence/mlu_over_time.jpg", height = 5, width = 6, dpi = 300)


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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
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
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight"){
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(subject, session, line, age, speaker, num_tokens) %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(subject, session, line, age, speaker, num_tokens)  %>%
      mutate(item = paste0(i), 
             variant = case_when(
               i %in% CDL_variants ~ "CDL", 
               i %in% ADL_variants ~ "ADL"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_mlu[[i]] <- utts_w_target
  
}

mlu <- do.call(rbind, get_mlu)
mlu$variant <- factor(mlu$variant, levels = c("CDL", "ADL"))

# higher MLUw for ADL variants
# increase in MLUw across time
# trending negative interaction with time
m <- lmer(num_tokens ~ variant*age + (1|item) + (1|subject), data = mlu)
summary(m)

mlu_byword <- mlu %>%
  group_by(item) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            variant = variant) %>%
  distinct()

mlu_byword_summary <- mlu_byword %>%
  group_by(variant) %>%
  summarize(mean = mean(mlu), 
            se = sd(mlu)/sqrt(length(mlu)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = mlu_byword, aes(x = variant, y = mlu, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = mlu_byword, aes(x = variant, y = mlu), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = mlu_byword_summary, aes(x = variant, y = mean, ymin = mean-se, ymax = mean+se, color = variant, fill = variant), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "variant", y = "MLUw", title = "LDP") +
  scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/LDP/mlu_overall.jpg", height = 5, width = 4, dpi = 300)

# higher MLUw for ADL variants
t.test(mlu ~ variant, data = mlu_byword, paired = TRUE)
shapiro.test(filter(mlu_byword, variant == "CDL")$mlu) #check for normality
shapiro.test(filter(mlu_byword, variant == "ADL")$mlu) 

mlu %>%
  group_by(item, age) %>%
  summarize(mlu = mean(num_tokens), 
            pair = pair, 
            variant = variant) %>%
  distinct() %>%
  ungroup() %>%
  group_by(age, variant) %>%
  summarize(mlu = mean(mlu)) %>%
  ggplot(aes(x = age, y = mlu, color = variant, fill = variant)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "MLUw", title = "LDP") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/LDP/mlu_over_time.jpg", height = 5, width = 6, dpi = 300)
