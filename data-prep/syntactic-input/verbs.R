library(childesr)
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)

path_to_verb_info = "data-prep/syntactic-input/parsed/"
parsed_filenames <- list.files(path = path_to_verb_info, pattern = "*.csv")
verb_files <- lapply(paste0(path_to_verb_info, parsed_filenames), read_csv)

for (i in c("other", "child")) {
  verb_info <- do.call(rbind, verb_files) %>%
    mutate(verbs = str_count(parsed_gloss, fixed("(VP ")) +
             str_count(parsed_gloss, fixed("(VBP ")) -
             str_count(parsed_gloss, fixed("(VP (VBP "))) %>%
    distinct()
  
  filename <- ifelse(i == "child", "data/input/verbs-child.csv", 
                     "data/input/verbs.csv")
  
  write_csv(as.data.frame(as.matrix(verb_info)), filename)
}