library(e1071)
library(caTools)
library(randomForest)
library(caret)
library(tidyverse)
library(tidymodels)
library(kernlab)
library(randomForestExplainer)
library(ROCR)
library(xgboost)

data <- read_csv("data/input/combined-other.csv") %>%
  select(form, rarity, complexity_wordbank, num_tokens, verbs, 
         pitch_mean, pitch_range, rate) %>%
  filter(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  pitch_mean, pitch_range, rate), ~ !is.na(.))) %>%
  mutate(form = case_when(
    form == "CDS" ~ 0, 
    form == "ADS" ~ 1))

#write_csv(data, "data/input/combined-other-no-na.csv")

data <- data %>%
  mutate(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  pitch_mean, pitch_range, rate), ~ scale(.)))

set.seed(123)
data_split <- initial_split(data)
train <- training(data_split)
test <- testing(data_split)

training_data <- as.matrix(train[,-1])
training_labels <- as.matrix(train[,1])

testing_data <- as.matrix(test[,-1])
testing_labels <- as.matrix(test[,1])

dtrain <- xgb.DMatrix(data = training_data, label = training_labels)
dtest <- xgb.DMatrix(data = testing_data, label = testing_labels)

negative_cases <- sum(training_labels == FALSE)
postive_cases <- sum(training_labels == TRUE)

model <- xgboost(data = dtrain, 
                 max.depth = 3,
                 nround = 50, 
                 early_stopping_rounds = 3,
                 objective = "binary:logistic", 
                 scale_pos_weight = negative_cases/postive_cases)

pred <- predict(model, dtest)
err <- mean(as.numeric(pred > 0.5) != testing_labels)
print(paste("test-error=", err))

xgb.plot.multi.trees(feature_names = names(training_data), 
                     model = model)

odds_to_probs <- function(odds) {
  return(exp(odds) / (1 + exp(odds)))
}

importance_matrix <- xgb.importance(names(training_data), model = model)
xgb.plot.importance(importance_matrix)

set.seed(234)
folds <- vfold_cv(train)
nested_cv(train, folds, inside = bootstraps(times = 5))

library(usemodels)
use_ranger(form ~ ., data = train) %>%
  # fill in info from k nearest neighbors model
  step_knnimpute(rarity, complexity_wordbank, pitch_mean, 
                 pitch_range, rate, num_tokens, verbs)

ranger_recipe <- 
  recipe(formula = form ~ ., data = train) 

ranger_spec <- 
  rand_forest(mtry = tune(), 
              min_n = tune(), 
              trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(35656)
ranger_tune <-
  tune_grid(ranger_workflow, 
            resamples = folds)

set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind == 1,]
test <- data[ind == 2,]

# convert form from chr to fct
train$form <- as.factor(train$form)
test$form <- as.factor(test$form)

# build random forest model
rf <- randomForest(form ~ ., data = train) 

predicted <- predict(rf, test)
confusionMatrix(predicted, test$form)


p1 <- predict(rf, train)
confusionMatrix(p1, train$form)

p2 <- predict(rf, test)
confusionMatrix(p2, test$form)

pred2 <- predict(rf, type = "prob")
perf <- prediction(pred2[,2], data$form)
auc <- performance(perf, "auc")
pred3 <- performance(perf, "tpr","fpr")
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

plot(rf)

train <- as.data.frame(train)

t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = TRUE,
           n.var = 7,
           main = "Variable Importance")
importance(rf)

partialPlot(rf, data, pitch_mean, "ADS")
partialPlot(rf, data, rarity, "ADS")


control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(123)
mtry <- sqrt(ncol(train))
rf_random <- train(form~., data=train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)