library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(ggpubr)

#get all utterances from providence corpus
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))

utterances <- childes_utterances %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '))

items <- read_csv("item_info/candidate_items_new.csv") %>%
  pull(word)

pairs <- read_csv("item_info/candidate_items_new.csv") %>%
  pull(pair)

#total number of words 
sum(utterances$num_tokens)

#loop over all cdi words for each utterance to get counts
for(i in items){
  if (i == "horsey"){
    utterances[str_detect(gloss, regex(paste0(' horsey | horsie '))), paste0(i) := str_count(gloss, regex(paste0(' horsey | horsie ')))]
  }
  
  else if (i == "doggy"){
    utterances[str_detect(gloss, regex(paste0(' doggy | doggie '))), paste0(i) := str_count(gloss, regex(paste0(' doggy | doggie ')))]
  }
  
  else if (i == "froggy"){
    utterances[str_detect(gloss, regex(paste0(' froggy | froggie '))), paste0(i) := str_count(gloss, regex(paste0(' froggy | froggie ')))]
  }
  
  else if (i == "duckie"){
    utterances[str_detect(gloss, regex(paste0(' duckie | ducky '))), paste0(i) := str_count(gloss, regex(paste0(' duckie | ducky ')))]
  }
  
  else if (i == "choo choo"){
    utterances[str_detect(gloss, regex(paste0(' choo choo | choo-choo '))), paste0(i) := str_count(gloss, regex(paste0(' choo choo | choo-choo ')))]
  }
  
  else if (i == "night night"){
    utterances[str_detect(gloss, regex(paste0(' night night | night-night '))), paste0(i) := str_count(gloss, regex(paste0(' night night | night-night ')))]
  }
  
  else if (i == "goodnight"){
    utterances[str_detect(gloss, regex(paste0(' goodnight | good night '))), paste0(i) := str_count(gloss, regex(paste0(' goodnight | good night ')))]
  }
  
  else if (i == "dolly"){
    utterances[str_detect(gloss, regex(paste0(' dolly | dollie '))), paste0(i) := str_count(gloss, regex(paste0(' dolly | dollie ')))]
  }
  
  else if (i == "piggy"){
    utterances[str_detect(gloss, regex(paste0(' piggy | piggie '))), paste0(i) := str_count(gloss, regex(paste0(' piggy | piggie ')))]
  }
  
  else if (i == "birdie"){
    utterances[str_detect(gloss, regex(paste0(' birdie | birdy '))), paste0(i) := str_count(gloss, regex(paste0(' birdie | birdy ')))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(' ',i,' '))), paste0(i) := str_count(gloss, regex(paste0(' ',i,' ')))]
}



#clean dataframe to get overall frequency by item
childes_data <- utterances[, 28:ncol(utterances)]
childes_data[is.na(childes_data)] <- 0
childes_freq <- data.frame(colSums(childes_data))
childes_freq <- setNames(cbind(rownames(childes_freq), childes_freq, row.names = NULL), c("word", "childes_freq"))


aoa <- read_csv("item_info/candidate_items_new.csv") %>%
  select(word, aoa, pair, form)

colors <- c("ids" = "#C1292E", "ads" = "#235789")
  
for (i in pairs) {
  ids <- paste(gsub("_.*", "", i))
  ads <- paste(gsub(".*_", "", i))
  
  plot <- utterances %>%
    filter(!is.na(eval(as.symbol(ids)))|!is.na(eval(as.symbol(ads)))) %>%
    select(gloss, stem, target_child_age, speaker_role, ids, ads) %>%
    mutate(age_rounded = round(target_child_age, digits=0), 
           speaker = case_when(
             speaker_role == "Target_Child" ~ "target_child", 
             speaker_role != "Target_Child" ~ "other_speaker")) %>%
    group_by(age_rounded) %>%
    summarise(ads = sum(eval(as.symbol(ads)), na.rm = TRUE),
              ids = sum(eval(as.symbol(ids)), na.rm = TRUE)) %>%
    pivot_longer(c(ids, ads), names_to = "form", values_to = "childes_freq") %>%
    mutate(word = case_when(
      form == "ids" ~ paste(gsub("_.*", "", i)), 
      form == "ads" ~ paste(gsub(".*_", "", i)))) %>%
    ggplot(aes(x=age_rounded, y=childes_freq, color=form, fill=form)) + 
    geom_vline(data = filter(aoa, word==paste(gsub("_.*", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_vline(data = filter(aoa, word==paste(gsub(".*_", "", i))), mapping = aes(xintercept=aoa, color=form)) +
    geom_point() +
    geom_smooth() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(x = "age (months)", y = "frequency") +
    theme_test() 
  
  assign(paste(i), plot)
}

library(ggpubr)

ggarrange(birdie_bird, blankie_blanket, bunny_rabbit, `choo choo_train`, daddy_dad, doggy_dog,
          dolly_doll, duckie_duck, froggy_frog, horsey_horse, kitty_cat, mommy_mom,
          `night night_goodnight`, piggy_pig, potty_bathroom, tummy_stomach, 
          common.legend = TRUE)

utterances %>%
  filter(!is.na(doggy)|!is.na(dog)) %>%
  select(gloss, stem, target_child_age, speaker_role, doggy, dog) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(doggy = sum(doggy, na.rm = TRUE),
            dog = sum(dog, na.rm = TRUE)) %>%
  pivot_longer(c(doggy, dog), names_to = "word", values_to = "childes_freq") %>%
  mutate(form = case_when(
    word == "doggy" ~ "ids", 
    word == "dog" ~ "ads")) %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=form, fill=form)) + 
  geom_vline(data = filter(aoa, word=="doggy"), mapping = aes(xintercept=aoa, color=form)) +
  geom_vline(data = filter(aoa, word=="dog"), mapping = aes(xintercept=aoa, color=form)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = "age (months)", y = "frequency") +
  theme_test() 

######
#items <- read_csv("candidate_items_new.csv") 

#merge(items, childes_freq, by="word") %>%
  #write_csv("item_info/merged.csv")

#summary <- read_csv("merged.csv") %>%
  #mutate(exclude = ifelse(is.na(exclude), "n", "y")) %>%
  #filter(exclude == "n") 

#summary %>%
  #group_by(form) %>%
  #summarise(mean_aoa = mean(aoa, na.rm = TRUE), 
            #mean_bab = mean(babiness, na.rm = TRUE))
