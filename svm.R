library(tidyverse)
library(e1071)
library(caTools)
library(caret)

speech_rate <- read_csv("data_prep/svm/speech_rate.csv")
pitch <- read_csv("data_prep/svm/pitch.csv")
syntactic_complexity <- read_csv("data_prep/svm/syntactic_complexity.csv")
mlu <- read_csv("data_prep/svm/mlu.csv")

early_pairs <- read_csv("data_prep/item_info.csv") %>%
  filter(shift_type == "early") %>%
  pull(pair)

late_pairs <- read_csv("data_prep/item_info.csv") %>%
  filter(shift_type == "late") %>%
  pull(pair)

never_pairs <- read_csv("data_prep/item_info.csv") %>%
  filter(shift_type == "never") %>%
  pull(pair)

merged <- full_join(speech_rate, pitch, by = c("item", "transcript_id", "corpus_name", "target_child_name", 
                                         "speaker_id", "age", "media_start", "media_end", "form", 
                                         "pair", "form_numeric")) %>%
  full_join(syntactic_complexity, by = c("transcript_id", "id", "age", "speaker_id", "num_tokens", 
                                         "item", "pair", "form", "form_numeric", "target_child_id")) %>%
  full_join(mlu, by = c("transcript_id", "id", "age", "speaker_id", "num_tokens", 
                        "item", "pair", "form", "form_numeric", 
                        "target_child_id", "speaker_role"))

early <- merged %>%
  filter(pair %in% early_pairs) %>% 
  select(id, form, speech_rate, pitch_mean, pitch_range, num_tokens, n_verbs, age) %>%
  na.omit() %>%
  group_by(id) %>%
  mutate(n = length(unique(form)), 
         form = ifelse(n > 1, "both", form)) %>%
  ungroup() %>%
  filter(form != "both") %>%
  transmute(form = as.factor(form), 
         speech_rate = scale(speech_rate),
         pitch_mean = scale(pitch_mean), 
         pitch_range = scale(pitch_range),
         n_tokens = scale(num_tokens), 
         n_verbs = scale(n_verbs), 
         age = scale(age))

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_early <- train(form ~ ., data = early, method = "svmLinear", 
              trControl = train_control)
svm_early

split = sample.split(early$form, SplitRatio = 0.75)

training_set = subset(early, split == TRUE)
test_set = subset(early, split == FALSE)

svmfit = svm(form ~ ., data = training_set, 
             type = "C-classification",
             kernel = "linear", 
             scale = FALSE, 
             cost = 10,
             class.weights = c("ADS" = 0.4254442, "CDS" = 0.5745558))
summary(svmfit)

pred <- predict(svmfit, newdata = test_set)
confusionMatrix(pred, test_set$form)

late <- merged %>%
  filter(pair %in% late_pairs) %>% 
  select(id, form, speech_rate, pitch_mean, pitch_range, num_tokens, n_verbs, age) %>%
  na.omit() %>%
  group_by(id) %>%
  mutate(n = length(unique(form)), 
         form = ifelse(n > 1, "both", form)) %>%
  ungroup() %>%
  filter(form != "both") %>%
  transmute(form = as.factor(form), 
            speech_rate = scale(speech_rate),
            pitch_mean = scale(pitch_mean), 
            pitch_range = scale(pitch_range),
            n_tokens = scale(num_tokens), 
            n_verbs = scale(n_verbs), 
            age = scale(age))

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_late <- train(form ~ ., data = late, method = "svmLinear", 
                   trControl = train_control)
svm_late

split = sample.split(late$form, SplitRatio = 0.75)

training_set = subset(late, split == TRUE)
test_set = subset(late, split == FALSE)

svmfit = svm(form ~ ., data = training_set, 
             type = "C-classification",
             kernel = "linear", 
             scale = FALSE, 
             cost = 10,
             class.weights = c("ADS" = 0.9155203, "CDS" = 0.08447975))
summary(svmfit)

pred <- predict(svmfit, newdata = test_set)
confusionMatrix(pred, test_set$form)

never <- merged %>%
  filter(pair %in% never_pairs) %>% 
  select(id, form, speech_rate, pitch_mean, pitch_range, num_tokens, n_verbs, age) %>%
  na.omit() %>%
  group_by(id) %>%
  mutate(n = length(unique(form)), 
         form = ifelse(n > 1, "both", form)) %>%
  ungroup() %>%
  filter(form != "both") %>%
  transmute(form = as.factor(form), 
            speech_rate = scale(speech_rate),
            pitch_mean = scale(pitch_mean), 
            pitch_range = scale(pitch_range),
            n_tokens = scale(num_tokens), 
            n_verbs = scale(n_verbs), 
            age = scale(age))

split = sample.split(never$form, SplitRatio = 0.75)

training_set = subset(never, split == TRUE)
test_set = subset(never, split == FALSE)

svmfit = svm(form ~ ., data = training_set, 
             type = "C-classification",
             kernel = "linear", 
             scale = FALSE, 
             cost = 10,
             class.weights = c("ADS" = 0.2750424, "CDS" = 0.7249576))
summary(svmfit)

pred <- predict(svmfit, newdata = test_set)
confusionMatrix(pred, test_set$form)