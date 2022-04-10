library(e1071)
library(caTools)
library(randomForest)
library(caret)
library(tidyverse)
library(tidymodels)
library(kernlab)
library(randomForestExplainer)
library(ROCR)

data <- read_csv("data/input/combined-other.csv") %>%
  select(form, rarity, complexity_wordbank, num_tokens, verbs, 
         pitch_mean, pitch_range, rate) %>%
  filter(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  pitch_mean, pitch_range, rate), ~ !is.na(.)))

write_csv(data, "data/input/combined-other-no-na.csv")

data <- data %>%
  mutate(across(c(rarity, complexity_wordbank, num_tokens, verbs, 
                  pitch_mean, pitch_range, rate), ~ scale(.)))

set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.9, 0.1))
train <- data[ind == 1,]
test <- data[ind == 2,]

# convert form from chr to fct
train$form <- as.factor(train$form)
test$form <- as.factor(test$form)
data$form <- as.factor(data$form)

rf <- randomForest(form ~ ., data = data, localImp = TRUE) 

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

data <- as.data.frame(data)

t <- tuneRF(data[,-1], data[,1],
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

