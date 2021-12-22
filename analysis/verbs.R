library(childesr)
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

source("data-prep/overall/functions.R")
get_utts_w_target(childes_utterances, items)

verbs <- read_csv("data-prep/parser/other-utterances-parsed.csv") %>%
  bind_rows("data-prep/parser/child-utterances-parsed.csv") %>%
  mutate(age = round(target_child_age, digits = 0),
         n_verbs = str_count(parsed_gloss, fixed("(VP ")) +
           str_count(parsed_gloss, fixed("(VBP ")) -
           str_count(parsed_gloss, fixed("(VP (VBP "))) %>%
  select(id, n_verbs)

verbs_other <- utterances %>%
  filter(speaker_type == "other") %>%
  left_join(verbs, by = "id") %>%
  mutate(age_scaled = scale(age),
         verbs_scaled = scale(n_verbs))

verbs_child <- utterances %>%
  filter(speaker_type == "child") %>%
  left_join(verbs, by = "id") %>%
  mutate(age_scaled = scale(age),
         verbs_scaled = scale(n_verbs))

# predict form from verbs: other-produced utts ------------------------------
m <- glmer(form_numeric ~ verbs_scaled * age_scaled + 
             (1|pair) + 
             (1|speaker_id), 
           data = verbs_other, 
           family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
summary(m)
save_model_output(m, "analysis/model-outputs/other-utts/verbs.csv")

verbs_byword <- verbs_other %>%
  group_by(item) %>%
  summarize(verbs = mean(n_verbs), 
            pair = pair, 
            form = form) %>%
  distinct()

verbs_byword_summary <- verbs_byword %>%
  group_by(form) %>%
  summarize(mean = mean(verbs), 
            se = sd(verbs)/sqrt(length(verbs)), 
            ymin = mean - se, 
            ymax = mean + se)

ggplot() +
  geom_line(data = verbs_byword, aes(x = form, y = verbs, group = pair), 
            color = "#F2F2F2", size = 1) +
  geom_point(data = verbs_byword, aes(x = form, y = verbs), 
             color = "#F2F2F2", size = 2) +
  geom_pointrange(data = verbs_byword_summary, 
                  aes(x = form, y = mean, 
                      ymin = mean - se, ymax = mean + se, 
                      color = form, fill = form), 
                  stat = "identity", size = 1.25) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(x = "Form", y = "Number of Verb Phrases") +
  scale_y_continuous(limits = c(3, 9), breaks = seq(3, 9, by = 3)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("writing/figs/input/verbs.jpg", height = 5, width = 4, dpi = 300)