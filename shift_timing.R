library(childesr)
library(tidyverse)
library(data.table)
library(ggpubr)
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

colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

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
# (for each time pt, what is the probability of producing CDL vs. ADL variant?)
for (i in pairs) {
  CDL <- paste(gsub("_.*", "", i))
  ADL <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDL)))|!is.na(eval(as.symbol(ADL)))) %>%
    select(age, speaker_role, CDL, ADL) %>%
    group_by(age) %>%
    summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
              ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE)) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDL" ~ paste(gsub("_.*", "", i)), 
      variant == "ADL" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDL" ~ 0, 
      variant == "ADL" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDL_count = length(variant[variant=="CDL"]),
              ADL_count = length(variant[variant=="ADL"]), 
              CDL = CDL_count/(CDL_count + ADL_count),
              ADL = ADL_count/(CDL_count + ADL_count)) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "prop") %>%
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
  CDL <- paste(gsub("_.*", "", i))
  ADL <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDL)))|!is.na(eval(as.symbol(ADL)))) %>%
    select(age, speaker_role, CDL, ADL) %>%
    group_by(age) %>%
    summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
              ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE), 
              total_tokens = CDL + ADL) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDL" ~ paste(gsub("_.*", "", i)), 
      variant == "ADL" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDL" ~ 0, 
      variant == "ADL" ~ 1))
  
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

# TEMPORARY
overall_trend_adults <- overall_trend

ggplot() + 
  #geom_smooth(data=model_data_long, aes(x=age, y=variant_numeric, group=pair), method="glm", method.args=list(family = "binomial"), 
             # color="#F5F5F5", se=FALSE) +
  #geom_ribbon(data=overall_trend_adults, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              #fill="#4B0082", alpha=0.25) +
  #geom_ribbon(data=overall_trend, aes(x=x, ymin=predicted-conf.low, ymax=predicted+conf.low), 
              #fill="#235789", alpha=0.25) +
  geom_line(data=overall_trend, aes(x=x, y=predicted), color="#235789", size = 2) +
  geom_line(data=overall_trend_adults, aes(x=x, y=predicted), color="#4B0082", size = 2) +
  scale_x_continuous(limits = c(0, 84), breaks=seq(0, 84, by=12)) +
  labs(x = "Age (months)", y = "Probability of producing ADL variant") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/ADL_over_time_with_adults.jpg")


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
# (for each time pt, what is the probability of producing CDL vs. ADL variant?)
for (i in pairs) {
  CDL <- paste(gsub("_.*", "", i))
  ADL <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(CDL)))|!is.na(eval(as.symbol(ADL)))) %>%
    select(age, speaker_role, CDL, ADL) %>%
    group_by(age) %>%
    summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
              ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE)) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDL" ~ paste(gsub("_.*", "", i)), 
      variant == "ADL" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDL" ~ 0, 
      variant == "ADL" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDL_count = length(variant[variant=="CDL"]),
              ADL_count = length(variant[variant=="ADL"]), 
              CDL = CDL_count/(CDL_count + ADL_count),
              ADL = ADL_count/(CDL_count + ADL_count)) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "prop") %>%
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
  CDL <- paste(gsub("_.*", "", i))
  ADL <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(CDL)))|!is.na(eval(as.symbol(ADL)))) %>%
    select(age, speaker_role, CDL, ADL) %>%
    group_by(age) %>%
    summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
              ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE), 
              total_tokens = CDL + ADL) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDL" ~ paste(gsub("_.*", "", i)), 
      variant == "ADL" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDL" ~ 0, 
      variant == "ADL" ~ 1))
  
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
  labs(x = "Age (months)", y = "Probability of producing ADL variant", 
       title = "Providence") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/Providence/ADL_over_time.jpg")


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
# (for each time pt, what is the probability of producing CDL vs. ADL variant?)
for (i in pairs) {
  CDL <- paste(gsub("_.*", "", i))
  ADL <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDL)))|!is.na(eval(as.symbol(ADL)))) %>%
    select(age, CDL, ADL) %>%
    group_by(age) %>%
    summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
              ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE)) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDL" ~ paste(gsub("_.*", "", i)), 
      variant == "ADL" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDL" ~ 0, 
      variant == "ADL" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDL_count = length(variant[variant=="CDL"]),
              ADL_count = length(variant[variant=="ADL"]), 
              CDL = CDL_count/(CDL_count + ADL_count),
              ADL = ADL_count/(CDL_count + ADL_count)) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "prop") %>%
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
  CDL <- paste(gsub("_.*", "", i))
  ADL <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDL)))|!is.na(eval(as.symbol(ADL)))) %>%
    select(age, CDL, ADL) %>%
    group_by(age) %>%
    summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
              ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE), 
              total_tokens = CDL + ADL) %>%
    pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
    mutate(word = case_when(
      variant == "CDL" ~ paste(gsub("_.*", "", i)), 
      variant == "ADL" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(variant_numeric = case_when(
      variant == "CDL" ~ 0, 
      variant == "ADL" ~ 1))
  
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
  labs(x = "Age (months)", y = "Probability of producing ADL variant", 
       title = "LDP") +
  geom_hline(yintercept=0.5, linetype="dotted", size=1) +
  theme_test(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0, 1))
ggsave("figs/LDP/ADL_over_time.jpg")