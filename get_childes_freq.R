library(childesr)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)

#get all utterances from providence corpus
childes_utterances = data.table(get_utterances(collection = "Eng-NA")) %>%
  filter(target_child_age<60)

utterances <- childes_utterances %>%
  mutate(gloss = paste0(' ', tolower(gloss), ' '))

items <- read_csv("candidate_items_new.csv") %>%
  pull(word)

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
  
  else if (i == "baa baa"){
    utterances[str_detect(gloss, regex(paste0(' baa baa | baa-baa '))), paste0(i) := str_count(gloss, regex(paste0(' baa baa | baa-baa ')))]
  }
  
  else if (i == "duckie"){
    utterances[str_detect(gloss, regex(paste0(' duckie | ducky '))), paste0(i) := str_count(gloss, regex(paste0(' duckie | ducky ')))]
  }
  
  else if (i == "boo boo"){
    utterances[str_detect(gloss, regex(paste0(' boo boo | boo-boo '))), paste0(i) := str_count(gloss, regex(paste0(' boo boo | boo-boo ')))]
  }
  
  else if (i == "vroom vroom"){
    utterances[str_detect(gloss, regex(paste0(' vroom vroom | vroom-vroom '))), paste0(i) := str_count(gloss, regex(paste0(' vroom vroom | vroom-vroom ')))]
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
  
  else if (i == "din din"){
    utterances[str_detect(gloss, regex(paste0(' din din | din-din '))), paste0(i) := str_count(gloss, regex(paste0(' din din | din-din ')))]
  }
  
  else utterances[str_detect(gloss, regex(paste0(' ',i,' '))), paste0(i) := str_count(gloss, regex(paste0(' ',i,' ')))]
}



#clean dataframe to get overall frequency by item
childes_data <- utterances[, 28:ncol(utterances)]
childes_data[is.na(childes_data)] <- 0
childes_freq <- data.frame(colSums(childes_data))
childes_freq <- setNames(cbind(rownames(childes_freq), childes_freq, row.names = NULL), c("word", "childes_freq"))

dog <- utterances %>%
  filter(!is.na(dog)|!is.na(doggy)) %>%
  select(gloss, stem, target_child_age, speaker_role, dog, doggy) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(dog = sum(dog, na.rm = TRUE),
            doggy = sum(doggy, na.rm = TRUE)) %>%
  pivot_longer(c(dog, doggy), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
    geom_vline(xintercept = 16) +
    geom_point() +
    geom_smooth() +
    scale_color_manual(values = c("#235789", "#C1292E")) +
    scale_fill_manual(values = c("#235789", "#C1292E")) +
    labs(x = "age (months)", y = "frequency") +
    theme_test()
    
train <- utterances %>%
  filter(!is.na(`choo choo`)|!is.na(train)) %>%
  select(gloss, stem, target_child_age, speaker_role, `choo choo`, train) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(`choo choo` = sum(`choo choo`, na.rm = TRUE),
            train = sum(train, na.rm = TRUE)) %>%
  pivot_longer(c(`choo choo`, train), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 21) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#C1292E", "#235789")) +
  scale_fill_manual(values = c("#C1292E", "#235789")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()

cat <- utterances %>%
  filter(!is.na(kitty)|!is.na(cat)) %>%
  select(gloss, stem, target_child_age, speaker_role, kitty, cat) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(kitty = sum(kitty, na.rm = TRUE),
            cat = sum(cat, na.rm = TRUE)) %>%
  pivot_longer(c(kitty, cat), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 18) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#235789", "#C1292E")) +
  scale_fill_manual(values = c("#235789", "#C1292E")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()

frog <- utterances %>%
  filter(!is.na(froggy)|!is.na(frog)) %>%
  select(gloss, stem, target_child_age, speaker_role, froggy, frog) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(froggy = sum(froggy, na.rm = TRUE),
            frog = sum(frog, na.rm = TRUE)) %>%
  pivot_longer(c(froggy, frog), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 22) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#235789", "#C1292E")) +
  scale_fill_manual(values = c("#235789", "#C1292E")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()

blanket <- utterances %>%
  filter(!is.na(blankie)|!is.na(blanket)) %>%
  select(gloss, stem, target_child_age, speaker_role, blankie, blanket) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(blankie = sum(blankie, na.rm = TRUE),
            blanket = sum(blanket, na.rm = TRUE)) %>%
  pivot_longer(c(blankie, blanket), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 21) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#235789", "#C1292E")) +
  scale_fill_manual(values = c("#235789", "#C1292E")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()

bathroom <- utterances %>%
  filter(!is.na(potty)|!is.na(bathroom)) %>%
  select(gloss, stem, target_child_age, speaker_role, potty, bathroom) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(potty = sum(potty, na.rm = TRUE),
            bathroom = sum(bathroom, na.rm = TRUE)) %>%
  pivot_longer(c(potty, bathroom), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 24) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#235789", "#C1292E")) +
  scale_fill_manual(values = c("#235789", "#C1292E")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()

doll <- utterances %>%
  filter(!is.na(dolly)|!is.na(doll)) %>%
  select(gloss, stem, target_child_age, speaker_role, dolly, doll) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(dolly = sum(dolly, na.rm = TRUE),
            doll = sum(doll, na.rm = TRUE)) %>%
  pivot_longer(c(dolly, doll), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 22) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#235789", "#C1292E")) +
  scale_fill_manual(values = c("#235789", "#C1292E")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()


vroom <- utterances %>%
  filter(!is.na(`baa baa`)) %>%
  select(gloss, stem, target_child_age, speaker_role, potty, bathroom) %>%
  mutate(age_rounded = round(target_child_age, digits=0), 
         speaker = case_when(
           speaker_role == "Target_Child" ~ "target_child", 
           speaker_role != "Target_Child" ~ "other_speaker")) %>%
  group_by(age_rounded) %>%
  summarise(`vroom vroom` = sum(`vroom vroom`, na.rm = TRUE),
            car = sum(car, na.rm = TRUE)) %>%
  pivot_longer(c(`vroom vroom`, car), names_to = "word", values_to = "childes_freq") %>%
  ggplot(aes(x=age_rounded, y=childes_freq, color=word, fill=word)) + 
  geom_vline(xintercept = 24) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("#235789", "#C1292E")) +
  scale_fill_manual(values = c("#235789", "#C1292E")) +
  labs(x = "age (months)", y = "frequency") +
  theme_test()


library(ggpubr)
ggarrange(dog, cat, frog, train, blanket, bathroom, 
          labels = c("dog", "cat", "frog", "train", "blanket", "bathroom"))

######
items <- read_csv("candidate_items_new.csv") 

merge(items, childes_freq, by="word") %>%
  write_csv("merged.csv")

summary <- read_csv("merged.csv") %>%
  mutate(exclude = ifelse(is.na(exclude), "n", "y")) %>%
  filter(exclude == "n") 

summary %>%
  group_by(form) %>%
  summarise(mean_aoa = mean(aoa, na.rm = TRUE), 
            mean_bab = mean(babiness, na.rm = TRUE))
