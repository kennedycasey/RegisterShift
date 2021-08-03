library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)

# get all utterances from providence corpus
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

aoa <- read_csv("item_info/candidate_items_new.csv") %>%
  select(word, aoa, pair, form)

colors <- c("ids" = "#C1292E", "ads" = "#235789")

### rerun on providence subset
providence_utterances <- childes_utterances %>%
  filter(target_child_age < 60 & corpus_name == "Providence") %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age_rounded = round(target_child_age, digits = 0))


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




# clean dataframe to get overall frequency by item
providence_data <- providence_utterances[, 28:ncol(utterances)]
providence_data[is.na(providence_data)] <- 0
providence_freq <- data.frame(colSums(providence_data))
providence_freq <- setNames(cbind(rownames(providence_freq), providence_freq, row.names = NULL), c("word", "providence_freq"))

# generate raw freq plots
for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  plot <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(gloss, stem, age_rounded, speaker_role, ids, ads) %>%
    group_by(age_rounded) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE),
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "childes_freq") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    ggplot(aes(x=age_rounded, y=providence_freq, color=form, fill=form)) + 
    geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_point() +
    geom_smooth() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  
  assign(paste(i), plot)
}

raw_frequency <- ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, daddy_dad, doggy_dog,
                           dolly_doll, duckie_duck, froggy_frog, horsey_horse, kitty_cat, mommy_mom,
                           `night night_goodnight`, piggy_pig, potty_bathroom, tummy_stomach, 
                           common.legend = TRUE, legend = "top", 
                           ncol = 3, nrow = 5)

annotate_figure(raw_frequency, 
                left = text_grob("frequency", rot = 90, size = 25), 
                bottom = text_grob("age (months)", size = 25))

ggsave("plots/childes/providence/providence_raw_frequency.jpg", height = 15, width = 20, dpi = 300)


# generate relative item-level freq plots 
# (for each word, how many times was it said in a given month, 
# relative to the total number of times it was said from 0-72m)

for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  plot <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(gloss, stem, age_rounded, speaker_role, ids, ads) %>%
    group_by(age_rounded) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE),
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)) %>%
    mutate(ids_total = filter(providence_freq, word==paste(gsub("_.*", "", i)))$providence_freq, 
           ads_total = filter(providence_freq, word==paste(gsub(".*_", "", i)))$providence_freq, 
           ids = ids/ids_total, 
           ads = ads/ads_total) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "providence_freq_relative") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    ggplot(aes(x=age_rounded, y=providence_freq_relative, color=form, fill=form)) + 
    geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_point() +
    geom_smooth() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  
  assign(paste(i), plot)
}


per_item_freq <- ggarrange(birdie_bird, doggy_dog, bunny_rabbit, 
                           blankie_blanket, horsey_horse, daddy_dad, 
                           dolly_doll, kitty_cat, mommy_mom, 
                           duckie_duck, `night night_goodnight`, potty_bathroom, 
                           froggy_frog, piggy_pig, tummy_stomach, 
                           common.legend = TRUE, legend = "bottom", 
                           ncol = 3, nrow = 5)

annotate_figure(per_item_freq, 
                top = text_grob("ads always dominates      ads takes over early        ads takes over late", size = 25, face = "bold"),
                left = text_grob("relative item-level frequency", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("age (months)", size = 25, face = "bold"))

ggsave("plots/childes/providence/providence_per_item_frequency.jpg", height = 15, width = 12, dpi = 300)

# generate prop plots
# (for each timepoint, what is the proportion of ids vs. ads forms)

for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(age_rounded, speaker_role, ids, ads) %>%
    group_by(age_rounded) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE),
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "count") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(form_numeric = case_when(
      form == "ids" ~ 0, 
      form == "ads" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age_rounded) %>%
    summarise(ids_count = length(form[form=="ids"]),
              ads_count = length(form[form=="ads"]), 
              ids = ids_count/(ids_count + ads_count),
              ads = ads_count/(ids_count + ads_count)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "prop") %>%
    ggplot(aes(x=age_rounded, y=prop, color=form, fill=form)) + 
    geom_point() +
    geom_smooth(data=model_data_long, aes(x=age_rounded, y=form_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#235789", fill="#235789") +
    geom_smooth(data=model_data_long %>% mutate(form_numeric = case_when(form_numeric==1 ~ 0, form_numeric==0 ~ 1)), 
                aes(x=age_rounded, y=form_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#C1292E", fill="#C1292E") +
    geom_hline(yintercept=0.5, linetype="dotted", size=1) +
    geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  assign(paste(i), plot)
}


prop <- ggarrange(birdie_bird, doggy_dog, bunny_rabbit, 
                  blankie_blanket, horsey_horse, daddy_dad, 
                  dolly_doll, kitty_cat, mommy_mom, 
                  duckie_duck, `night night_goodnight`, potty_bathroom, 
                  froggy_frog, piggy_pig, tummy_stomach, 
                  common.legend = TRUE, legend = "bottom", 
                  ncol = 3, nrow = 5)

annotate_figure(prop, 
                top = text_grob("ads always dominates      ads takes over early        ads takes over late", size = 25, face = "bold"),
                left = text_grob("proportion of tokens per form", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("age (months)", size = 25, face = "bold"))

ggsave("plots/childes/providence/providence_props.jpg", height = 15, width = 12, dpi = 300)


# generate odds ratio plots
# (for each timepoint, what are the odds of producing ids vs. ads form)

for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(age_rounded, speaker_role, ids, ads) %>%
    group_by(age_rounded) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE),
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "count") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(form_numeric = case_when(
      form == "ids" ~ 0, 
      form == "ads" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:5]
  
  plot <- model_data_long %>%
    group_by(age_rounded) %>%
    summarise(ids_count = length(form[form=="ids"]),
              ads_count = length(form[form=="ads"]), 
              ids_odds = ids_count/(ids_count + ads_count),
              ads_odds = ads_count/(ids_count + ads_count), 
              or = ifelse(ads_odds == 0 | ids_odds == 0, NA, ads_odds/ids_odds)) %>%
    ggplot(aes(x=age_rounded, y=or)) + 
    geom_point() +
    geom_smooth(method="glm", color="#235789", fill="#235789") +
    geom_hline(yintercept=1, linetype="dotted", size=1) +
    labs(title = paste0(i)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none") +
    coord_cartesian(ylim=c(0, 20))
  
  assign(paste(i), plot)
}


odds <- ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, daddy_dad, doggy_dog,
                  dolly_doll, duckie_duck, froggy_frog, horsey_horse, kitty_cat, mommy_mom,
                  `night night_goodnight`, piggy_pig, potty_bathroom, tummy_stomach, 
                  common.legend = TRUE, legend = "top", 
                  ncol = 3, nrow = 5)

annotate_figure(odds, 
                left = text_grob("odds ratio", rot = 90, size = 25), 
                bottom = text_grob("age (months)", size = 25))

ggsave("plots/childes/providence/providence_odds.jpg", height = 15, width = 20, dpi = 300)


# generate prop plots - compare children vs. adults
# (for each timepoint, what is the proportion of ids vs. ads forms)

for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  model_data <- providence_utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(age_rounded, speaker_role, ids, ads) %>% 
    mutate(speaker = ifelse(speaker_role == "Target_Child", "target child", "other speaker")) %>%
    group_by(age_rounded, speaker) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE),
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "count") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(form_numeric = case_when(
      form == "ids" ~ 0, 
      form == "ads" ~ 1))
  
  model_data_long <- model_data[rep(row.names(model_data), model_data$count), 1:6]
  
  plot <- model_data_long %>%
    group_by(age_rounded, speaker) %>%
    summarise(ids_count = length(form[form=="ids"]),
              ads_count = length(form[form=="ads"]), 
              ids = ids_count/(ids_count + ads_count),
              ads = ads_count/(ids_count + ads_count)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "prop") %>%
    ggplot(aes(x=age_rounded, y=prop, color=form, fill=form)) + 
    facet_grid(.~speaker) +
    geom_point() +
    geom_smooth(data=model_data_long, aes(x=age_rounded, y=form_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#235789", fill="#235789") +
    geom_smooth(data=model_data_long %>% mutate(form_numeric = case_when(form_numeric==1 ~ 0, form_numeric==0 ~ 1)), 
                aes(x=age_rounded, y=form_numeric), 
                method="glm", method.args=list(family = "binomial"), 
                color="#C1292E", fill="#C1292E") +
    geom_hline(yintercept=0.5, linetype="dotted", size=1) +
    geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  assign(paste(i), plot)
}

prop_by_speaker <- ggarrange(birdie_bird, doggy_dog, bunny_rabbit, 
                             blankie_blanket, horsey_horse, daddy_dad, 
                             dolly_doll, kitty_cat, mommy_mom, 
                             duckie_duck, `night night_goodnight`, potty_bathroom, 
                             froggy_frog, piggy_pig, tummy_stomach, 
                             common.legend = TRUE, legend = "bottom", 
                             ncol = 3, nrow = 5)

annotate_figure(prop_by_speaker,
                left = text_grob("proportion of tokens per form", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("age (months)", size = 25, face = "bold"))

ggsave("plots/childes/providence/providence_props_by_speaker.jpg", height = 15, width = 15, dpi = 300)


# generate relative age-level freq plots 
# (for each word, how many times was it said in a given month, 
# relative to the total number of words heard during that month)

month_totals <- providence_utterances %>%
  mutate(age_rounded = round(target_child_age, digits=0)) %>%
  group_by(age_rounded) %>%
  summarise(month_total = sum(num_tokens, na.rm = TRUE))

totaled_utterances <- merge(providence_utterances, month_totals, by="age_rounded")

for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  plot <- totaled_utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(gloss, stem, age_rounded, speaker_role, ids, ads, month_total) %>%
    group_by(age_rounded) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE)/month_total*1000000,
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)/month_total*1000000) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "providence_freq_relative") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    ungroup() %>%
    ggplot(aes(x=age_rounded, y=providence_freq_relative, color=form, fill=form)) + 
    geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_point() +
    geom_smooth() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(i)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  
  assign(paste(i), plot)
}


per_age_frequency <- ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, daddy_dad, doggy_dog,
                               dolly_doll, duckie_duck, froggy_frog, horsey_horse, kitty_cat, mommy_mom,
                               `night night_goodnight`, piggy_pig, potty_bathroom, tummy_stomach, 
                               common.legend = TRUE, legend = "top", 
                               ncol = 3, nrow = 5)

annotate_figure(per_age_frequency, 
                left = text_grob("relative frequency per 1 million words", rot = 90, size = 25), 
                bottom = text_grob("age (months)", size = 25))

ggsave("plots/childes/providence/providence_per_age_frequency.jpg", height = 15, width = 20, dpi = 300)
