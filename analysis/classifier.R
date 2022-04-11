library(caret)
library(tidyverse)
library(xgboost)

data <- read_csv("data/input/combined-other.csv") %>%
  select(form, rarity, complexity_wordbank, num_tokens, verbs, 
         pitch_mean, pitch_range, rate) %>%
  #filter(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  #pitch_mean, pitch_range, rate), ~ !is.na(.))) %>%
  mutate(form = case_when(
    form == "CDS" ~ 0, 
    form == "ADS" ~ 1))

#write_csv(data, "data/input/combined-other-no-na.csv")

data <- data %>%
  mutate(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  pitch_mean, pitch_range, rate), ~ scale(.)))

set.seed(123)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

training_data <- as.matrix(train[,-1])
training_labels <- as.matrix(train[,1])

testing_data <- as.matrix(test[,-1])
testing_labels <- as.matrix(test[,1])

dtrain <- xgb.DMatrix(data = training_data, label = training_labels)
dtest <- xgb.DMatrix(data = testing_data, label = testing_labels)

# account for imbalanced sample
negative_cases <- sum(training_labels == FALSE)
postive_cases <- sum(training_labels == TRUE)

model <- xgboost(data = dtrain, 
                 max.depth = 6,
                 nround = 50, 
                 eta = 0.3, # learning rate
                 gamma = 10, # regularization control
                 objective = "binary:logistic", 
                 scale_pos_weight = negative_cases/postive_cases, 
                 eval_metric = "error")

pred_heldout <- predict(model, dtest)

err <- mean(as.numeric(pred_heldout > 0.5) != testing_labels)
print(paste("test-error=", err))

pred <- predict(model, dtrain)

# combined decision tree
xgb.plot.multi.trees(feature_names = names(training_data), 
                     model = model)

# feature importance (average gain in predictive ability)
importance_matrix <- xgb.importance(names(training_data), model = model)
xgb.plot.importance(importance_matrix)

# AUC for held-out set
plot.roc(test$form, pred_heldout, 
         print.auc = TRUE, print.auc.y = 0.5) 

# confusion matrix
table(test$form, as.numeric(pred_heldout > 0.5))

# AUC for training set
plot.roc(train$form, pred, 
         print.auc = TRUE, print.auc.y = 0.5) 