library(tidyverse)

# INPUT PREDICTORS --------------------------------------------------------
path <- "analysis/model-outputs/input-predictors/"
filenames <- list.files(path, "*.csv")

for (i in c("child", "other")) {
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
                  width = 0, size = 0.75, color = "black") + 
    geom_hline(yintercept = 0, size = 0.75, linetype = "dotted", 
               color = "black", alpha = 0.5) +
    scale_alpha_manual(values = c("not_sig" = 0.2, "sig" = 0.8)) + 
    scale_color_manual(values = input_colors) +
    scale_fill_manual(values = input_colors) +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = "Linguistic Predictor", y = "Coefficient Estimate") +
    theme_test(base_size = 10) +
    theme(legend.position = "none")
  
  ggsave(paste0("presentations/CDS/figs/input-predictors-", i, ".png"), 
         dpi = 300, width = 5, height = 5)
}