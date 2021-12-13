library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(ggeffects)
library(sjPlot)

# set overall parameters ----
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

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

# get mlu for adult-produced utterances ----
utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role != "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# create empty list to be populated
get_verbs <- list() 
# loop over all items to get part of speech for all utterances containing a target word
for (i in items) {
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, speaker_id, num_tokens, part_of_speech) %>%
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
      select(target_child_id, transcript_id, id, age, speaker_role, speaker_id, num_tokens, part_of_speech) %>%
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
      select(target_child_id, transcript_id, id, age, speaker_role, speaker_id, num_tokens, part_of_speech) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "night night") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, speaker_id, num_tokens, part_of_speech) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else if (i == "goodnight") {
    utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" goodnight | good night | good-night ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, speaker_id, num_tokens, part_of_speech) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  }
  
  else utts_w_target <- utterances %>%
      filter(str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))) %>%
      select(target_child_id, transcript_id, id, age, speaker_role, speaker_id, num_tokens, part_of_speech) %>%
      mutate(item = paste0(i), 
             form = case_when(
               i %in% CDS_forms ~ "CDS", 
               i %in% ADS_forms ~ "ADS"), 
             pair = paste0((filter(pairs, word == i))$pair))
  
  get_verbs[[i]] <- utts_w_target
}

verbs <- do.call(rbind, get_verbs) %>%
  mutate(form = factor(form, levels = c("CDS", "ADS")), 
         form_numeric = case_when(
           form == "CDS" ~ 0, 
           form == "ADS" ~ 1), 
         part_of_speech = paste0(" ", trimws(part_of_speech), " "), 
         n_verbs = str_count(part_of_speech, " v "),
         age_scaled = scale(age), 
         n_verbs_scaled = scale(n_verbs))

write.csv(data.frame(verbs), "syntactic_complexity.csv", row.names = FALSE)

# does mlu predict CDS vs. ADS form? ----
m <- glmer(form_numeric ~ n_verbs_scaled * age_scaled + (1 + n_verbs_scaled *age_scaled|pair) + (1 + n_verbs_scaled * age_scaled|speaker_id), 
           data = verbs, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)

# old plots - clean up later ----
tab_model(m)
overall_trend <- ggpredict(m, c("mlu_scaled [all]"), type = "random")

ggplot() + 
  geom_smooth(data=mlu, aes(x=mlu_scaled, y=form_numeric, group=pair), method="glm", method.args=list(family = "binomial"),
              color="#F5F5F5", se=FALSE) +
  #geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              #fill="#235789", alpha=0.25) +
  #geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  labs(x = "Number of words in utterance (scaled)", y = "Probability of utterance containing ADS form") +
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
  #geom_smooth(data=model_data_long, aes(x=age, y=form_numeric, group=pair), method="glm", method.args=list(family = "binomial"), 
  #color="#F5F5F5", se=FALSE) +
  geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  #scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS form", 
       title = "CHILDES") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/ADS_over_time_no_items.jpg")



m <- glmer(form_numeric ~ mlu_scaled * age_scaled + (1|target_child_id), 
           data = filter(mlu, pair == "tummy_stomach"), 
           family = binomial, 
           control=glmerControl(optimizer="bobyqa"))
summary(m)


ggplot(mlu, aes(x = pair, y = num_tokens, color = form, fill = form)) +
  geom_jitter(alpha = 0.1) +
  geom_half_violin(data = (filter(mlu, form == "ADS")), aes(x = pair, y = num_tokens, color = form, fill = form), 
                   stat = "half_ydensity", side = "r", alpha = 0.5) +
  geom_half_violin(data = (filter(mlu, form == "CDS")), aes(x = pair, y = num_tokens, color = form, fill = form), 
                   stat = "half_ydensity", side = "l", alpha = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Item-Pair", y = "Number of Tokens Per Utterance", title = "CHILDES") +
  #scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggplot(mlu, aes(x = age, y = num_tokens, color = form, fill = form)) +
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
            form = form, 
            pair = pair, 
            age = age) %>%
  distinct()

ggplot(filter(mlu_by_session, mlu < 30), aes(x = age, y = mlu, color = form, fill = form)) +
  facet_wrap(.~pair, nrow = 3) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Age (months)", y = "MLUw", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


# higher MLUw for ADS forms
# increase in MLUw across time
# negative interaction with time
m <- lmer(num_tokens ~ form*age + (1|item) + (1|target_child_id), data = mlu)
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
  geom_line(data = mlu_byword, aes(x = form, y = mlu, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = mlu_byword, aes(x = form, y = mlu), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = mlu_byword_summary, aes(x = form, y = mean, ymin = mean-se, ymax = mean+se, color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "form", y = "MLUw", title = "CHILDES") +
  scale_y_continuous(limits = c(3, 9), breaks=seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/mlu_overall.jpg", height = 5, width = 4, dpi = 300)

# higher MLUw for ADS forms
t.test(mlu ~ form, data = mlu_byword, paired = TRUE)
shapiro.test(filter(mlu_byword, form == "CDS")$mlu) #check for normality
shapiro.test(filter(mlu_byword, form == "ADS")$mlu) 

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
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  scale_y_continuous(limits = c(2, 13), breaks=seq(0, 12, by = 3)) +
  labs(x = "Age (months)", y = "MLUw", title = "CHILDES") +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("figs/mlu_over_time.jpg", height = 5, width = 6, dpi = 300)

# nnet ----
library(nnet)
shift_type <- read_csv("data_prep/item_info.csv") %>%
  select(pair, shift_type)

mlu <- mlu %>%
  left_join(shift_type, by = "pair")

test <- multinom(shift_type ~ mlu_scaled, data = mlu)
summary(test)
coefs <- coef(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

coefs %>%
  add_column(z)

set.seed(222)
data <- mlu %>% select(shift_type, num_tokens)
data$shift_type <- as.factor(data$shift_type)
trainingRows <- sample(1:nrow(data), 0.6*nrow(data))
training <- data[trainingRows, ]
test <- data[-trainingRows, ]

multinomModel <- multinom(shift_type ~ num_tokens, data = training) # multinom Model
summary(multinomModel)

predicted_scores <- predict(multinomModel, test, "probs")
predicted_class <- predict(multinomModel, test)
table(predicted_class, test$num_tokens)
mean(as.character(predicted_class) != as.character(test$num_tokens))

library(VGAM)
fit <- vglm(shift_type~num_tokens, family=multinomial, data=data)
matrix(fit@coefficients, ncol = 2)
summary(fit)
probabilities <- predict(fit, data, type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(data$shift_type)[1]
predictions[which(predictions=="2")] <- levels(data$shift_type)[2]

table(predictions, data$shift_type)

early_model <- glmer(form_numeric ~ mlu_scaled * age_scaled + (1|target_child_id) + (1|pair),
                     family = "binomial",
                     control = glmerControl(optimizer="bobyqa"),
                     data = filter(mlu, pair %in% early_pairs))
summary(early_model)



late_model <- glmer(form_numeric ~ mlu_scaled * age_scaled + (1|target_child_id) + (1|pair),
                    family = "binomial",
                    control = glmerControl(optimizer="bobyqa"),
                    data = filter(mlu, pair %in% late_pairs))
summary(late_model)


never_model <- glmer(form_numeric ~ mlu_scaled * age_scaled + (1|target_child_id) + (1|pair),
                     family = "binomial",
                     control = glmerControl(optimizer="bobyqa"),
                     data = filter(mlu, pair %in% never_pairs))
summary(never_model)


test_data <- mlu %>%
  mutate(shift_type = case_when(
    pair %in% early_pairs ~ "early", 
    pair %in% late_pairs ~ "late", 
    pair %in% never_pairs ~ "never", 
  ))
test <- aov(num_tokens ~ shift_type, data = test_data)
summary(test)