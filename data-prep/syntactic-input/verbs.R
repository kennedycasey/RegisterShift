library(childesr)
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

source("data-prep/overall/functions.R")
get_utts_w_target(childes_utterances, items)

path_to_verb_info = "data-prep/parser/"
parsed_filenames <- list.files(path = path_to_verb_info, pattern = "*.csv")
verb_files <- lapply(paste0(path_to_verb_info, parsed_filenames), read_csv)
verb_info <- do.call(rbind, verb_files) %>%
  mutate(n_verbs = str_count(parsed_gloss, fixed("(VP ")) +
           str_count(parsed_gloss, fixed("(VBP ")) -
           str_count(parsed_gloss, fixed("(VP (VBP "))) %>%
  select(id, n_verbs) %>%
  distinct()

verbs_other <- utterances %>%
  filter(speaker_type == "other") %>%
  left_join(verb_info, by = "id") %>%
  mutate(age_scaled = scale(age), 
         verbs_scaled = scale(n_verbs))

verbs_child <- utterances %>%
  filter(speaker_type == "child") %>%
  left_join(verb_info, by = "id") %>%
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
  summarize(verbs = mean(n_verbs, na.rm = TRUE), 
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
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  theme_test(base_size = 20) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
ggsave("writing/figs/input/verbs.jpg", height = 5, width = 4, dpi = 300)