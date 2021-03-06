library(tidyverse)
library(lme4)
library(broom.mixed)

speaker_types <- c("child", "other")
colors <- c("CDL" = "#C1292E", "ADL" = "#235789")
input_colors <- c("Prosodic" = "#1C9E78", 
                  "Lexical" = "#D95F06", 
                  "Syntactic" = "#7570B4")

# INPUT PREDICTORS --------------------------------------------------------
path <- "analysis/model-outputs/input-predictors/"
filenames <- list.files(path, "*.csv")

for (i in speaker_types) {
  files <- lapply(paste0(path, 
                         filenames[!str_detect(filenames, 
                                               ifelse(i == "other", "-child|ratings", 
                                                      "-other|ratings"))]), read_csv)
  
  input.models <- bind_rows(files, .id = "source") %>%
    rename(Predictor = term, 
           Estimate = estimate, 
           SE = std.error) %>%
    mutate(Predictor = str_remove(str_replace(Predictor, "num_tokens", "length"), "_wordbank|_ratings")) %>%
    group_by(source) %>%
    mutate(order = row_number(), 
           type = case_when(
             order == 1 ~ "intercept", 
             order == 2 ~ "main_effect", 
             order == 3 ~ "age", 
             order == 4 ~ "interaction"), 
           Sig = ifelse(p.value < .05, "sig", "not_sig"), 
           Predictor = trimws(str_to_sentence(str_replace(Predictor, "_", " "), "first")), 
           level = ifelse(str_detect(Predictor, "Length|Verbs"), "Syntactic", 
                          ifelse(str_detect(Predictor, "Complexity|Rarity"), "Lexical", "Prosodic"))) %>%
    filter(type == "main_effect") %>%
    #mutate(Predictor = factor(Predictor, levels = c("Verbs", "Verbs:age", "Length", "Length:age", "Rarity", "Rarity:age", "Complexity", "Complexity:age", "Rate", "Rate:age", "Pitch range", "Pitch range:age", "Pitch mean", "Pitch mean:age")))
    mutate(Predictor = factor(Predictor, levels = c("Pitch mean", "Pitch range", "Rate", "Complexity",  "Rarity", "Length", "Verbs")))
    
  ggplot(input.models,
         aes(y = Predictor, x = Estimate, color = level)) +
    # geom_bar(aes(y = Predictor, x = Estimate, alpha = Sig, 
    #              color = level, fill = level), 
    #          stat = "identity") + 
    # geom_errorbar(aes(xmin = Estimate - SE, xmax = Estimate + SE), 
    #               width = 0.15, color = "black") + 
    geom_vline(xintercept = 0, size = 0.75, linetype = "dotted", 
               color = "black", alpha = 0.5) +
    scale_alpha_manual(values = c("not_sig" = 0.2, "sig" = 0.8)) + 
    scale_color_manual(values = input_colors) +
    scale_fill_manual(values = input_colors) +
    scale_x_continuous(limits = c(-0.65, 0.65), 
                       breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
    labs(y = "Linguistic Predictor", x = "Coefficient Estimate") +
    theme_test(base_size = 15) +
    theme(legend.position = "none")
  ggsave(paste0("presentations/CDS/figs/input-predictors-empty.png"), 
         dpi = 300, width = 5, height = 5)
  
  
  ggsave(paste0("presentations/CDS/figs/input-predictors-", i, ".png"), 
         dpi = 300, width = 5, height = 5)
}

# CHECK ISOLATION ---------------------------------------------------------
for (i in speaker_types) {
  data <- read_csv(paste0("data/input/pitch", 
                          ifelse(i == "child", "-child", ""), ".csv")) %>%
    mutate(isolated = ifelse(item == "night night" & num_tokens == 2, "isolated", 
                             ifelse(num_tokens == 1, "isolated", "not_isolated")), 
           form = factor(form, levels = c("CDS", "ADS"), labels = c("CDL", "ADL")), 
           pair = gsub(".*_", "", pair))
  
  model <- glmer(form_numeric ~ scale(pitch_mean) * scale(age) * isolated + 
                   (1|pair) + 
                   (1|speaker_id), 
                 data = filter(data, !is.na(pitch_mean)), 
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
  
  tidy(model) %>%
    filter(effect == "fixed") %>%
    write_csv(paste0("presentations/CDS/model-outputs/isolated-mean-pitch-", i, ".csv"))
  
  model <- glmer(form_numeric ~ scale(pitch_range) * scale(age) *isolated + 
                   (1|pair) + 
                   (1|speaker_id), 
                 data = filter(data, !is.na(pitch_range)), 
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
  
  tidy(model) %>%
    filter(effect == "fixed") %>%
    write_csv(paste0("presentations/CDS/model-outputs/isolated-pitch-range-", i, ".csv"))
  
  
  ggplot(filter(data, isolated == "isolated"), aes(x = pair, y = pitch_mean, color = form, fill = form)) +
    geom_boxplot(alpha = 0.2) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(x = "Pair", y = "Mean Pitch (Hz)", color = "Form", fill = "Form") +
    theme_classic(base_size = 8) 
  ggsave(paste0("presentations/CDS/figs/mean-pitch-isolated-", i, ".png"),
         dpi = 300, width = 7, height = 5)
  
  ggplot(filter(data, isolated == "not_isolated"), aes(x = pair, y = pitch_mean, color = form, fill = form)) +
    geom_boxplot(alpha = 0.2) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(x = "Pair", y = "Mean Pitch (Hz)", color = "Form", fill = "Form") +
    theme_classic(base_size = 8) 
  ggsave(paste0("presentations/CDS/figs/mean-pitch-not-isolated-", i, ".png"),
         dpi = 300, width = 7, height = 5)
}

# FORM CO-OCCURRENCE OVER AGE ---------------------------------------------
bytranscript_type <- read_csv("data/full-input.csv") %>%
  mutate(speaker_type = paste0(str_to_sentence(speaker_type), "-Produced Utterances")) %>%
  group_by(corpus_name, transcript_id, age, speaker_type) %>%
  mutate(total_utts = n()) %>%
  group_by(corpus_name, transcript_id, age, speaker_type, total_utts, form) %>%
  summarize(utts_w_form = n()) %>%
  pivot_wider(names_from = "form", values_from = "utts_w_form") %>%
  mutate(across(c("CDL", "ADL"), ~ ifelse(is.na(.), 0, ./total_utts)), 
         type = ifelse(CDL == 1, "CDL only", 
                       ifelse(ADL == 1, "ADL only", "Both"))) 

type <- bytranscript_type %>%
  ungroup() %>%
  group_by(age, speaker_type) %>%
  mutate(n_transcripts = n()) %>%
  group_by(age, speaker_type, n_transcripts, type) %>%
  summarize(prop = n()/n_transcripts) %>%
  distinct() 

colors3 <- c("CDL only" = "#C1292E", 
             "ADL only" = "#235789", 
             "Both" = "black")

ggplot(type, aes(x = age, y = prop*100, color = type, fill = type)) +
  facet_wrap(.~speaker_type) + 
  geom_jitter(bytranscript_type, mapping = aes(x = age, y = ADL*100), 
             color = "#235789", fill = "#235789", alpha = 0.02) + 
  geom_jitter(bytranscript_type, mapping = aes(x = age, y = CDL*100), 
             color = "#C1292E", fill = "#C1292E", alpha = 0.02) + 
  geom_smooth(size = 1.5) + 
  geom_point(size = 2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  labs(x = "Age (months)", y = "% Transcripts", 
       color = "Forms Detected", fill = "Forms Detected") +
  theme_test(base_size = 15)
ggsave("figs/transcript-level-cooccurrence.jpg", dpi = 300, width = 8, height = 5)



# SPEAKER CONTRIBUTION ----------------------------------------------------
byspeaker_tokens <- read_csv("data/full-input.csv") %>% 
  select(speaker_type, speaker_id) %>%
  distinct() %>%
  mutate(both = NA, 
         one = NA, 
         none = NA)

raw_data <- read_csv("data/full-input.csv")
for (i in 1:nrow(byspeaker_tokens)) {
  data <- raw_data %>%
    filter(speaker_id == byspeaker_tokens[i,]$speaker_id) %>%
    count(pair, form) %>%
    group_by(pair) %>%
    count()
  
  both_forms <- data %>%
    filter(n == 2) %>%
    nrow()
  
  one_form <- data %>%
    filter(n == 1) %>%
    nrow()
  
  neither_form <- 15 - both_forms - one_form
  
  byspeaker_tokens[i,"both"] <- both_forms
  
  byspeaker_tokens[i,"one"] <- one_form
  
  byspeaker_tokens[i,"none"] <- neither_form
}

byspeaker_tokens %>%
  mutate(speaker_type = paste0(str_to_sentence(speaker_type), "-Produced Utterances")) %>%
  pivot_longer(c(both, one, none), names_to = "form_count", values_to = "n_pairs") %>%
  mutate(form_count = case_when(
    form_count == "both" ~ 2, 
    form_count == "one" ~ 1, 
    form_count == "none" ~ 0)) %>%
  ggplot(aes(x = as.factor(form_count), n_pairs)) + 
  facet_wrap(.~speaker_type) +
  geom_jitter(alpha = 0.05) + 
  geom_violin() + 
  labs(x = "Number of Forms", y = "Number of Word Pairs") + 
  theme_test(base_size = 15)
ggsave("figs/tokens-by-speaker.jpg", dpi = 300, width = 6, height = 5)


# COMPLEXITY --------------------------------------------------------------
# complexity measures significantly correlated for others but not kids
other <- read_csv("data/input/combined-other.csv") %>%
  select(starts_with("complexity"))
cor.test(other$complexity_ratings, other$complexity_wordbank)

child <- read_csv("data/input/combined-child.csv") %>%
  select(starts_with("complexity"))
cor.test(child$complexity_ratings, child$complexity_wordbank)