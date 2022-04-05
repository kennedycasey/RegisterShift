library(e1071)
library(caTools)
library(caret)
library(tidyverse)

data <- read_csv("data/input/combined-other.csv") %>%
  select(form, rarity, complexity_wordbank, num_tokens, verbs, 
         pitch_mean, pitch_range, rate) %>%
  filter(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  pitch_mean, pitch_range, rate), ~ !is.na(.)))

set.seed(123)
split = sample.split(data$form, SplitRatio = 0.75)

training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

#training_set[-1] = scale(training_set[-1])
training_set[[1]] = factor(training_set[[1]])
#test_set[-1] = scale(test_set[-1])
test_set[[1]] = factor(test_set[[1]])


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm.model <- train(form ~., 
                   data = training_set, 
                   method = "svmLinear",
                   trControl = trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
svm_Linear_Grid <- train(form ~., data = training_set, method = "svmLinear",
                         trControl = trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred <- predict(svm.model, newdata = test_set)
test_pred

confusionMatrix(table(test_pred, test_set$form))

test_pred_grid <- predict(svm_Linear_Grid, newdata = test_set)
test_pred_grid

confusionMatrix(table(test_pred_grid, test_set$form))

