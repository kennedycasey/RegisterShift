library(childesr)
library(tidyverse)
library(data.table)
library(ggpubr)
library(grid)
library(lme4)
library(ggeffects)

# read in childes and ldp data
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances <- read.csv("~/Desktop/secure/ldp_data_prepped.csv") 

# set overall parameters
items <- read_csv("data_prep/item_info.csv") %>%
  pull(word)

pairs <- read_csv("data_prep/item_info.csv") %>%
  pull(pair)

aoa <- read_csv("data_prep/item_info.csv") %>%
  select(word, aoa, pair, variant)

colors <- c("CDS" = "#C1292E", "ADS" = "#235789")

# CHILDES -----------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role == "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

# get overall token counts
for(i in items){

  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                              root, "eys | ", root, "ies | ",
                                              root, "ey's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                          root, "eys | ", root, "ies | ",
                                                          root, "ey's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (i == "night night"){
    utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
               paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
  }
  
  else if (i == "goodnight"){
    utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
               paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                  paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
}

childes_data <- utterances[, 28:ncol(utterances)]
childes_data[is.na(childes_data)] <- 0
childes_freq <- data.frame(colSums(childes_data))
childes_freq <- setNames(cbind(rownames(childes_freq), childes_freq, row.names = NULL), c("word", "childes_freq"))

# generate prop plots
# (for each time pt, what is the probability of producing CDS vs. ADS variant?)

get_model_data <- list() 
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDS)))|!is.na(eval(as.symbol(ADS)))) %>%
    select(age, speaker_role, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE)) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDS" ~ paste(gsub("_.*", "", i)), 
      variant == "ADS" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDS" ~ 0, 
      variant == "ADS" ~ 1), 
      pair = paste0(i)) 
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  get_model_data[[i]] <- model_data_long
  
  m <- glm(variant_numeric ~ age, data = model_data_long, family = binomial)
  summary(m)
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDS_count = length(variant[variant=="CDS"]),
              ADS_count = length(variant[variant=="ADS"]), 
              CDS = CDS_count/(CDS_count + ADS_count),
              ADS = ADS_count/(CDS_count + ADS_count)) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "prop") %>%
    ggplot(aes(x=age, y=prop, color=variant, fill=variant)) + 
    geom_point() +
    geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#235789", fill="#235789") +
    geom_smooth(data=model_data_long %>% mutate(variant_numeric = case_when(variant_numeric==1 ~ 0, variant_numeric==0 ~ 1)), 
                aes(x=age, y=variant_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#C1292E", fill="#C1292E") +
    geom_hline(yintercept=0.5, linetype="dotted", size=1) +
    #geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=variant)) +
    #geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=variant)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i), color = "Variant", fill = "Variant") +
    scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  assign(paste(i), plot)
}

model_data <- do.call(rbind, get_model_data)

m <- glmer(variant_numeric ~ age + (1|pair), data = model_data, 
           family = binomial)
summary(m)

prop <- ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, 
                  daddy_dad, doggy_dog, dolly_doll, duckie_duck, 
                  froggy_frog, horsey_horse, kitty_cat,
                  mommy_mom, `night night_goodnight`, piggy_pig, 
                  potty_bathroom, tummy_stomach, 
                  common.legend = TRUE, legend = "bottom", 
                  ncol = 5, nrow = 3)

annotate_figure(prop,
                left = text_grob("Proportion of tokens per variant", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("Age (months)", size = 25, face = "bold"))

ggsave("figs/byItem_variants_over_time.jpg", height = 10, width = 20, dpi = 300)

# generate summary prop plot
model_data_list = list()
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDS)))|!is.na(eval(as.symbol(ADS)))) %>%
    select(age, speaker_role, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE), 
              total_tokens = CDS + ADS) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDS" ~ paste(gsub("_.*", "", i)), 
      variant == "ADS" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDS" ~ 0, 
      variant == "ADS" ~ 1))
  
  model_data_list[[i]] <- model_data
  }
model_data_merged <- do.call(rbind, model_data_list)
rownames(model_data_merged) <- 1:nrow(model_data_merged)

model_data_long <- model_data_merged[rep(row.names(model_data_merged), model_data_merged$count), 1:7]

m <- glmer(variant_numeric ~ scale(age) + (1|pair) + (1|total_tokens), #singular model fit if random slope for pair included
         family = "binomial",
         control = glmerControl(optimizer="bobyqa"),
         data = model_data_long)
summary(m)

overall_trend <- ggpredict(m, c("age [all]"), type = "random")


ggplot() + 
  geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric, group=pair), method="glm", method.args=list(family = "binomial"), 
              color="#F5F5F5", se=FALSE) +
  geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS variant") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/ADS_over_time.jpg")



# shift type analyses
early_pairs <- read_csv("data_prep/item_info.csv") %>%
  filter(shift_type == "early") %>%
  pull(pair)

early_model <- glmer(variant_numeric ~ scale(age) + (1|pair) + (1|total_tokens),
           family = "binomial",
           control = glmerControl(optimizer="bobyqa"),
           data = filter(model_data_long, pair %in% early_pairs))
summary(early_model)

early_trend <- ggpredict(early_model, c("age [all]"), type = "random")


early <- ggplot() + 
  geom_smooth((data=filter(model_data_long, pair %in% early_pairs)), mapping = aes(x=age, y=variant_numeric, group=pair), 
              method="glm", method.args=list(family = "binomial"), 
              color="#F5F5F5", se=FALSE) +
  geom_ribbon(data=early_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  geom_line(data=early_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS variant", 
       title = "Early shift to ADS") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_blank()) +
  coord_cartesian(ylim=c(0, 1))

late_pairs <- read_csv("data_prep/item_info.csv") %>%
  filter(shift_type == "late") %>%
  pull(pair)

late_model <- glmer(variant_numeric ~ scale(age) + (1|pair) + (1|total_tokens),
                     family = "binomial",
                     control = glmerControl(optimizer="bobyqa"),
                     data = filter(model_data_long, pair %in% late_pairs))
summary(late_model)

late_trend <- ggpredict(late_model, c("age [all]"), type = "random")


late <- ggplot() + 
  geom_smooth((data=filter(model_data_long, pair %in% late_pairs)), mapping = aes(x=age, y=variant_numeric, group=pair), 
              method="glm", method.args=list(family = "binomial"), 
              color="#F5F5F5", se=FALSE) +
  geom_ribbon(data=late_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  geom_line(data=late_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS variant", 
       title = "Late shift to ADS") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_blank()) +
  coord_cartesian(ylim=c(0, 1))


never_pairs <- read_csv("data_prep/item_info.csv") %>%
  filter(shift_type == "never") %>%
  pull(pair)

never_model <- glmer(variant_numeric ~ scale(age) + (1|pair) + (1|total_tokens),
                    family = "binomial",
                    control = glmerControl(optimizer="bobyqa"),
                    data = filter(model_data_long, pair %in% never_pairs))
summary(never_model)

never_trend <- ggpredict(never_model, c("age [all]"), type = "random")


never <- ggplot() + 
  geom_smooth((data=filter(model_data_long, pair %in% never_pairs)), mapping = aes(x=age, y=variant_numeric, group=pair), 
              method="glm", method.args=list(family = "binomial"), 
              color="#F5F5F5", se=FALSE) +
  geom_ribbon(data=never_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  geom_line(data=never_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS variant", 
       title = "ADS always dominates") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_blank()) +
  coord_cartesian(ylim=c(0, 1))

shift_types <- ggarrange(early, late, never, nrow = 1)

annotate_figure(shift_types, left = textGrob("Probability of producing ADS variant", rot = 90, vjust = 1, gp = gpar(cex = 1.7)),
                bottom = textGrob("Age (months)", gp = gpar(cex = 1.7)))
ggsave("figs/shift_types.jpg", width = 15.76, height = 8.42)



# Providence --------------------------------------------------------------
providence_utterances <- childes_utterances %>%
  filter(corpus_name == "Providence" & speaker_role == "Target_Child") %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0))

for(i in items){
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    providence_utterances[str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                         root, "eys | ", root, "ies | ",
                                                         root, "ey's | ", root, "ie's "))), 
                          paste0(i) := str_count(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                                     root, "eys | ", root, "ies | ",
                                                                     root, "ey's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    providence_utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's "))), 
                          paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                                     root, "ys | ", root, "ies | ",
                                                                     root, "y's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    providence_utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                         root, "ys | ", root, "ies | ",
                                                         root, "y's | ", root, "ie's "))), 
                          paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                                     root, "ys | ", root, "ies | ",
                                                                     root, "y's | ", root, "ie's ")))]
  }
  
  else if (i == "night night"){
    providence_utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
                          paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
  }
  
  else if (i == "goodnight"){
    providence_utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
                          paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
  }
  
  else providence_utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                             paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
}

# generate prop plots
# (for each time pt, what is the probability of producing CDS vs. ADS variant?)
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(CDS)))|!is.na(eval(as.symbol(ADS)))) %>%
    select(age, speaker_role, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE)) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDS" ~ paste(gsub("_.*", "", i)), 
      variant == "ADS" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDS" ~ 0, 
      variant == "ADS" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDS_count = length(variant[variant=="CDS"]),
              ADS_count = length(variant[variant=="ADS"]), 
              CDS = CDS_count/(CDS_count + ADS_count),
              ADS = ADS_count/(CDS_count + ADS_count)) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "prop") %>%
    ggplot(aes(x=age, y=prop, color=variant, fill=variant)) + 
    geom_point() +
    geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#235789", fill="#235789") +
    geom_smooth(data=model_data_long %>% mutate(variant_numeric = case_when(variant_numeric==1 ~ 0, variant_numeric==0 ~ 1)), 
                aes(x=age, y=variant_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#C1292E", fill="#C1292E") +
    geom_hline(yintercept=0.5, linetype="dotted", size=1) +
    #geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=variant)) +
    #geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=variant)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i), color = "variant", fill = "variant") +
    scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  assign(paste(i), plot)
}

prop <- ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, 
                  daddy_dad, doggy_dog, dolly_doll, duckie_duck, 
                  froggy_frog, horsey_horse, kitty_cat,
                  mommy_mom, `night night_goodnight`, piggy_pig, 
                  potty_bathroom, tummy_stomach, 
                  common.legend = TRUE, legend = "bottom", 
                  ncol = 5, nrow = 3)

annotate_figure(prop,
                left = text_grob("Proportion of tokens per variant", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("Age (months)", size = 25, face = "bold"))

ggsave("figs/Providence/byItem_variants_over_time.jpg", height = 10, width = 20, dpi = 300)

# generate summary prop plot
model_data_list = list()
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(CDS)))|!is.na(eval(as.symbol(ADS)))) %>%
    select(age, speaker_role, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE), 
              total_tokens = CDS + ADS) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDS" ~ paste(gsub("_.*", "", i)), 
      variant == "ADS" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDS" ~ 0, 
      variant == "ADS" ~ 1))
  
  model_data_list[[i]] <- model_data
}

model_data_merged <- do.call(rbind, model_data_list)
rownames(model_data_merged) <- 1:nrow(model_data_merged)

model_data_long <- model_data_merged[rep(row.names(model_data_merged), model_data_merged$count), 1:7]

m <- glmer(variant_numeric ~ scale(age) + (1|pair) + (1|total_tokens), #singular model fit if random slope for pair included
           family = "binomial",
           control = glmerControl(optimizer="bobyqa"),
           data = model_data_long)
summary(m)

overall_trend <- ggpredict(m, c("age [all]"), type = "re")

ggplot() + 
  geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric, group=pair), method="glm", method.args=list(family = "binomial"), 
              color="#F5F5F5", se=FALSE) +
  geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS variant", 
       title = "Providence") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/Providence/ADS_over_time.jpg")


# LDP ---------------------------------------------------------------------
utterances <- data.table(ldp_utterances) %>%
  filter(speaker=="target_child")

for(i in items){
  
  if (str_detect(i, "ey")) {
    root <- paste(gsub("ey", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                              root, "eys | ", root, "ies | ",
                                              root, "ey's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "ey | ", root, "ie | ",
                                                          root, "eys | ", root, "ies | ",
                                                          root, "ey's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "y") & !str_detect(i, "ey")) {
    root <- paste(gsub("y", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (str_detect(i, "ie")) {
    root <- paste(gsub("ie", "", i))
    utterances[str_detect(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                              root, "ys | ", root, "ies | ",
                                              root, "y's | ", root, "ie's "))), 
               paste0(i) := str_count(gloss, regex(paste0(" ", root, "y | ", root, "ie | ",
                                                          root, "ys | ", root, "ies | ",
                                                          root, "y's | ", root, "ie's ")))]
  }
  
  else if (i == "night night"){
    utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
               paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
  }
  
  else if (i == "goodnight"){
    utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
               paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                  paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
}

# generate prop plots
# (for each time pt, what is the probability of producing CDS vs. ADS variant?)
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDS)))|!is.na(eval(as.symbol(ADS)))) %>%
    select(age, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE)) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDS" ~ paste(gsub("_.*", "", i)), 
      variant == "ADS" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDS" ~ 0, 
      variant == "ADS" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDS_count = length(variant[variant=="CDS"]),
              ADS_count = length(variant[variant=="ADS"]), 
              CDS = CDS_count/(CDS_count + ADS_count),
              ADS = ADS_count/(CDS_count + ADS_count)) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "prop") %>%
    ggplot(aes(x=age, y=prop, color=variant, fill=variant)) + 
    geom_point() +
    geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#235789", fill="#235789") +
    geom_smooth(data=model_data_long %>% mutate(variant_numeric = case_when(variant_numeric==1 ~ 0, variant_numeric==0 ~ 1)), 
                aes(x=age, y=variant_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#C1292E", fill="#C1292E") +
    geom_hline(yintercept=0.5, linetype="dotted", size=1) +
    #geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=variant)) +
    #geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=variant)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i), color = "variant", fill = "variant") +
    scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  assign(paste(i), plot)
}

prop <- ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, 
                  daddy_dad, doggy_dog, dolly_doll, duckie_duck, 
                  froggy_frog, horsey_horse, kitty_cat,
                  mommy_mom, `night night_goodnight`, piggy_pig, 
                  potty_bathroom, tummy_stomach, 
                  common.legend = TRUE, legend = "bottom", 
                  ncol = 5, nrow = 3)

annotate_figure(prop,
                left = text_grob("Proportion of tokens per variant", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("Age (months)", size = 25, face = "bold"))

ggsave("figs/LDP/byItem_variants_over_time.jpg", height = 10, width = 20, dpi = 300)

# generate summary prop plot
model_data_list = list()
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDS)))|!is.na(eval(as.symbol(ADS)))) %>%
    select(age, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE), 
              total_tokens = CDS + ADS) %>%
    pivot_longer(c(CDS, ADS), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDS" ~ paste(gsub("_.*", "", i)), 
      variant == "ADS" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDS" ~ 0, 
      variant == "ADS" ~ 1))
  
  model_data_list[[i]] <- model_data
}
model_data_merged <- do.call(rbind, model_data_list)
rownames(model_data_merged) <- 1:nrow(model_data_merged)

model_data_long <- model_data_merged[rep(row.names(model_data_merged), model_data_merged$count), 1:7]

m <- glmer(variant_numeric ~ scale(age) + (1|pair) + (1|total_tokens), #singular model fit if random slope for pair included
           family = "binomial",
           control = glmerControl(optimizer="bobyqa"),
           data = model_data_long)
summary(m)

overall_trend <- ggpredict(m, c("age [all]"), type = "re")

ggplot() + 
  geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric, group=pair), method="glm", method.args=list(family = "binomial"), 
              color="#F5F5F5", se=FALSE) +
  geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              fill="#235789", alpha=0.25) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADS variant", 
       title = "LDP") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/LDP/ADS_over_time.jpg")