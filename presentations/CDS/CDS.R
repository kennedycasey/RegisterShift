library(tidyverse)
library(lme4)
library(broom.mixed)

speaker_types <- c("child", "other")
colors <- c("CDL" = "#C1292E", "ADL" = "#235789")

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
         aes(x = Predictor, y = Estimate, color = level)) +
    geom_bar(aes(x = Predictor, y = Estimate, alpha = Sig, 
                 color = level, fill = level), 
             stat = "identity") + 
    geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), 
                  width = 0.15, color = "black") + 
    geom_hline(yintercept = 0, size = 0.75, linetype = "dotted", 
               color = "black", alpha = 0.5) +
    scale_alpha_manual(values = c("not_sig" = 0.2, "sig" = 0.8)) + 
    scale_color_manual(values = input_colors) +
    scale_fill_manual(values = input_colors) +
    scale_y_continuous(limits = c(-0.65, 0.65), 
                       breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
    labs(x = "Linguistic Predictor", y = "Coefficient Estimate") +
    theme_test(base_size = 10) +
    theme(legend.position = "none")
  
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
