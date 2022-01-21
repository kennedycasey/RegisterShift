library(childesr)
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)
source("data-prep/overall/functions.R")

mlu_other <- utterances %>%
  filter(speaker_type == "other") %>%
  mutate(age_scaled = scale(age), 
           MLUw_scaled = scale(num_tokens), 
           MLUm_scaled = scale(num_morphemes))

mlu_child <- utterances %>%
  filter(speaker_type == "child") %>%
  mutate(age_scaled = scale(age), 
         MLUw_scaled = scale(num_tokens), 
         MLUm_scaled = scale(num_morphemes))

# predict form from MLU: other-produced utts ------------------------------
m <- glmer(form_numeric ~ MLUw_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_other, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/MLUw.csv")

mlu_byword <- mlu_other %>%
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
  geom_pointrange(data = mlu_byword_summary, 
                  aes(x = form, y = mean, 
                      ymin = mean - se, ymax = mean + se, 
                      color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "MLUw") +
  scale_y_continuous(limits = c(3, 9), breaks = seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("writing/figs/input/MLU.jpg", height = 5, width = 4, dpi = 300)

m <- glmer(form_numeric ~ MLUm_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_other, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/MLUm.csv")

# predict form from MLU: child-produced utts ------------------------------
m <- glmer(form_numeric ~ MLUw_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_child, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/child-utts/MLUw.csv")

m <- glmer(form_numeric ~ MLUm_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = mlu_child, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/child-utts/MLUm.csv")


# test data
library(ggridges)

early_pairs <- c("doggy_dog", "kitty_cat", 
                 "night night_goodnight", 
                 "dolly_doll", "horsey_horse")

late_pairs <- c("mommy_mom", "daddy_dad", 
                "tummy_stomach", "bunny_rabbit", 
                "potty_bathroom")

never_pairs <- c("birdie_bird", "piggy_pig", 
                 "froggy_frog", "blankie_blanket", 
                 "duckie_duck")

test <- mlu_other %>%
  mutate(shift_type = case_when(
    pair %in% early_pairs ~ "early", 
    pair %in% late_pairs ~ "late", 
    pair %in% never_pairs ~ "never"))
  

ggplot(test, aes(x = MLUw_scaled, y = shift_type, fill = form)) +
  geom_density_ridges(scale = 0.75, alpha = 0.5) +
  scale_fill_manual(values = colors) +
  theme_minimal()