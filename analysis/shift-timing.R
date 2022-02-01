library(childesr)
library(tidyverse)
library(data.table)
library(ggpubr)
library(grid)
library(lme4)
library(ggeffects)
library(broom.mixed)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances = data.table(read_csv("~/Desktop/secure/ldp_data_prepped.csv")) %>%
  filter(speaker == "target_child") %>%
  select(gloss, age)

# set overall parameters
items <- read_csv("data-prep/overall/item-info.csv") %>%
  pull(word)

pairs <- read_csv("data-prep/overall/item-info.csv") %>%
  pull(pair)

aoa <- read_csv("data-prep/overall/item-info.csv") %>%
  select(word, aoa, pair, form)

colors <- c("CDS" = "#C1292E", "ADS" = "#235789")

# CHILDES -----------------------------------------------------------------
utterances <- childes_utterances %>%
  filter(target_child_age < 84 & speaker_role == "Target_Child") %>% 
  mutate(gloss = paste0(' ', tolower(gloss), ' '), 
         age = round(target_child_age, digits = 0)) %>%
  select(gloss, age) %>%
  bind_rows(ldp_utterances)

# get overall token counts
for (i in items) {

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
  
  else if (i == "night night") {
    utterances[str_detect(gloss, regex(paste0(" night night | night-night | night nights | night-nights "))), 
               paste0(i) := str_count(gloss, regex(paste0(" night night | night-night | night nights | night-nights ")))]
  }
  
  else if (i == "goodnight") {
    utterances[str_detect(gloss, regex(paste0(" goodnight | good night | good-night "))), 
               paste0(i) := str_count(gloss, regex(paste0(" goodnight | good night | good-night ")))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s "))), 
                  paste0(i) := str_count(gloss, regex(paste0(" ", i, " | ", i, "s | ", i, "'s ")))]
}

childes_data <- utterances[, 3:ncol(utterances)]
childes_data[is.na(childes_data)] <- 0
childes_freq <- data.frame(colSums(childes_data))
childes_freq <- setNames(cbind(rownames(childes_freq), childes_freq, row.names = NULL), c("word", "childes_freq"))

# get count of child utterances with at least 1 target word
child.utts <- utterances
child.utts[is.na(child.utts)] <- 0

child.utts <- child.utts %>%
  mutate(sum = rowSums(across(doggy:bird))) %>%
  filter(sum >= 1) %>%
  nrow()



# generate prop plots
# (for each time pt, what is the probability of producing CDS vs. ADS form?)

get_model_data <- list() 
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDS))) | !is.na(eval(as.symbol(ADS)))) %>%
    select(age, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE)) %>%
    pivot_longer(c(CDS, ADS), names_to = "form", values_to = "count") %>%
    mutate(word = case_when(
      form == "CDS" ~ paste(gsub("_.*", "", i)), 
      form == "ADS" ~ paste(gsub(".*_", "", i)))) %>%
    distinct() %>%
    mutate(form_numeric = case_when(
      form == "CDS" ~ 0, 
      form == "ADS" ~ 1), 
      pair = paste0(i)) 
  
  model_data_long <- model_data[rep(seq(nrow(model_data)), model_data$count), ]
  
  get_model_data[[i]] <- model_data_long
  
  m <- glm(form_numeric ~ age, data = model_data_long, family = binomial)
  summary(m)
  
  plot <- model_data_long %>%
    group_by(age) %>%
    summarize(CDS_count = length(form[form == "CDS"]),
              ADS_count = length(form[form == "ADS"]), 
              CDS = CDS_count/(CDS_count + ADS_count),
              ADS = ADS_count/(CDS_count + ADS_count)) %>%
    pivot_longer(c(CDS, ADS), names_to = "form", values_to = "prop") %>%
    ggplot(aes(x = age, y = prop, color = form, fill = form)) + 
    geom_vline(xintercept = xintercept, color = "gray", size = 1.5) + 
    geom_point() +
    geom_smooth(data = model_data_long, 
                aes(x = age, y = form_numeric), 
                method = "glm", method.args = list(family = "binomial"), 
                color = "#235789", fill = "#235789") +
    geom_smooth(data = model_data_long %>% mutate(form_numeric = 
                                                  case_when(form_numeric == 1 ~ 0, 
                                                            form_numeric == 0 ~ 1)), 
                aes(x = age, y = form_numeric), 
                method = "glm", method.args = list(family = "binomial"), 
                color = "#C1292E", fill = "#C1292E") +
    geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = paste0(str_replace(i, "_", "/")), color = "form", fill = "form") +
    scale_x_continuous(limits = c(0, 84), breaks = seq(0, 84, by = 12)) +
    theme_test(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, size = 15), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none")
  
  assign(paste(i), plot)
}

model_data <- do.call(rbind, get_model_data)

m <- glmer(form_numeric ~ age + (1 + age|pair), data = model_data, 
           family = binomial)

for (i in unique(pairs)) {
  model_data_pair <- model_data %>%
    filter(pair == i)
}

prop <- ggarrange(birdie_bird, doggy_dog, bunny_rabbit,
                  blankie_blanket, dolly_doll, daddy_dad, 
                  duckie_duck, horsey_horse, mommy_mom, 
                  froggy_frog, kitty_cat, potty_bathroom, 
                  piggy_pig, `night night_goodnight`, tummy_stomach,
                  ncol = 3, nrow = 5)

annotate_figure(prop,
                top = text_grob("      No Shift                  Early Shift                Late Shift", size = 25, face = "bold"),
                left = text_grob("Proportion of tokens per form", rot = 90, size = 25, face = "bold"), 
                bottom = text_grob("Age (months)", size = 25, face = "bold"))

ggsave("writing/figs/bypair-shift-timing.png", height = 15, width = 10, dpi = 300)

# generate summary prop plot
model_data_list = list()
for (i in pairs) {
  CDS <- paste(gsub("_.*", "", i))
  ADS <- paste(gsub(".*_", "", i))
  
  model_data <- utterances %>%
    filter(!is.na(eval(as.symbol(CDS))) | !is.na(eval(as.symbol(ADS)))) %>%
    select(age, CDS, ADS) %>%
    group_by(age) %>%
    summarize(CDS = sum(eval(as.symbol(CDS)), na.rm = TRUE),
              ADS = sum(eval(as.symbol(ADS)), na.rm = TRUE), 
              total_tokens = CDS + ADS) %>%
    pivot_longer(c(CDS, ADS), names_to = "form", values_to = "count") %>%
    mutate(word = case_when(
      form == "CDS" ~ paste(gsub("_.*", "", i)), 
      form == "ADS" ~ paste(gsub(".*_", "", i))), 
      pair = paste(i)) %>%
    distinct() %>%
    mutate(form_numeric = case_when(
      form == "CDS" ~ 0, 
      form == "ADS" ~ 1))
  
  model_data_list[[i]] <- model_data
}

model_data_merged <- do.call(rbind, model_data_list)
rownames(model_data_merged) <- 1:nrow(model_data_merged)

model_data_long <- model_data_merged[rep(seq(nrow(model_data_merged)), model_data_merged$count), 1:7]

m <- glmer(form_numeric ~ scale(age) + (1 + scale(age)|pair),
         family = "binomial",
         control = glmerControl(optimizer = "bobyqa"),
         data = model_data_long)
summary(m)

tidy(m) %>%
  mutate(term = str_remove_all(term, "scale")) %>%
  filter(effect == "fixed" & term == "(age)") %>%
  write_csv("analysis/model-outputs/shift-timing.csv")

overall_trend <- ggpredict(m, c("age [all]"), type = "random") 

# compute the age at which ADL forms are produced >50% of the time 
xintercept <- overall_trend %>%
  filter(predicted >= 0.5) %>%
  slice_head() %>%
  pull(x)

ggplot() + 
  geom_smooth(data = model_data_long, 
              aes(x = age, y = form_numeric, group = pair), 
              method = "glm", method.args = list(family = "binomial"), 
              color = "#F5F5F5", se = FALSE) +
  geom_ribbon(data = overall_trend, 
              aes(x = x, ymin = predicted - conf.low, ymax = predicted + conf.low), 
              fill = "#235789", alpha = 0.25) +
  geom_line(data = overall_trend, 
            aes(x = x, y = predicted), color = "#235789", size = 2) +
  scale_x_continuous(limits = c(0, 84), breaks = seq(0, 84, by = 12)) +
  labs(x = "Age (months)", y = "Probability of producing ADL form") +
  geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
  theme_test(base_size = 20) +
  theme(axis.title = element_text(face = "bold")) +
  coord_cartesian(ylim = c(0, 1))
ggsave("writing/figs/shift-timing.png", height = 5, width = 6, dpi = 300)

model_outputs_list = list()
for (i in pairs) {
  bypair_model_data <- model_data_long %>% 
    filter(pair == i)
  
  model <- glm(form_numeric ~ scale(age),
               family = "binomial",
               data = bypair_model_data)
  
  output <- tidy(model) %>%
    mutate(term = str_remove_all(term, "scale")) %>%
    filter(term == "(age)")
  
  model_outputs_list[[i]] <- output
}

do.call(rbind, model_outputs_list) %>%
  rownames_to_column(var = "pair") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  write_csv("analysis/model-outputs/bypair-shift-timing.csv")