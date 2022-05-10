library(tidyverse)
library(caret)
library(xgboost)
library(tidymodels)
library(pROC)
library(xgboostExplainer)


# FUNCTIONS ---------------------------------------------------------------
create_xgboost_model <- function(train, test, cv, name, fname='', seed=42, graph=T) {
  set.seed(seed) # reproducible
  form = which(names(train)=="form") # class column number
  xgb.train.data = xgb.DMatrix(data.matrix(train[,-form]), label = train[,form], missing = NA)
  param <- list(objective = "binary:logistic", base_score = 0.5)
  xgboost.cv = xgb.cv(param=param, data = xgb.train.data, folds = cv, 
                      nrounds = 1000, early_stopping_rounds = 100, metrics='auc')
  best_iteration = xgboost.cv$best_iteration
  xgb.model <- xgboost(param=param, data=xgb.train.data, nrounds=best_iteration)
  xgb.test.data = xgb.DMatrix(data.matrix(test[,-form]), missing = NA)
  xgb.preds = predict(xgb.model, xgb.test.data)
  
  # ROC on the test set (do we want it on combined test and train set?)
  xgb.roc_obj <- roc(test[,form], xgb.preds)
  
  # add predicted column to both training and test sets
  train$pred = predict(xgb.model, xgb.train.data)
  train$set = "train"
  test$pred = xgb.preds
  test$set = "test"
  
  ttdat = rbind(train, test)
  ttdat$CDSpred = ifelse(ttdat$pred > .5, 1, 0)
  
  #xgb.train.roc_obj <- roc(train[,form], xgb.train.preds)
  #plot(xgb.train.roc_obj)
  #text(x=.5, y=.1, paste0("AUC = ",round(auc(xgb.train.roc_obj), 3))) # raw LENA AUC: .835
  if(graph) {
    if(fname == '') fname = ncol(train) - 1
    pdf(paste0("figs/xgb/xgbROC-", name, ".pdf"), width = 3.5, height = 3.5)
    plot(xgb.roc_obj)
    text(x = 0.5, y = 0.1, paste0("AUC = ", round(auc(xgb.roc_obj), 3))) 
    dev.off()
    
    #### XGB importance
    col_names = attr(xgb.train.data, ".Dimnames")[[2]]
    imp = xgb.importance(col_names, xgb.model)
    pdf(paste0("figs/xgb/xgb-importance-", name,".pdf"), width = 6, height = 6)
    xgb.plot.importance(imp)
    dev.off()
    
    importance_matrix <- xgb.importance(col_names, model = xgb.model)
    print(importance_matrix)
  }
  
  OOF_prediction <- data.frame(max_prob = round(xgb.preds), 
                               label = test$form) 
  
  cmOOF = confusionMatrix(factor(OOF_prediction$label), 
                          factor(OOF_prediction$max_prob),
                          mode = "everything")
  #print(paste("XGB AUC =", auc(xgb.roc_obj))) 
  
  print(cmOOF) 
  
  # should retrain on all data and save that model..
  
  return(list(model = xgb.model, 
              train.data = xgb.train.data, 
              test.data = xgb.test.data, 
              ttdat = ttdat, # train and test data with predicted
              cmOOF = cmOOF, 
              roc = xgb.roc_obj, preds = xgb.preds)) 
}

#### XGBoost Explainer
explain_xgboost_model <- function(xgb) {
  explainer = buildExplainer(xgb$model, xgb$train.data, type="binary", 
                             base_score = 0.5, trees_idx = NULL)
  # Error in while (currentnode > 0) { : argument is of length zero
  pred.breakdown = explainPredictions(xgb$model, explainer, xgb$test.data)
  cat('Breakdown Complete','\n')
  weights = rowSums(pred.breakdown)
  pred.xgb = 1/(1+exp(-weights))
  cat(max(xgb$preds-pred.xgb),'\n')
  return(list(explainer = explainer, 
              pred.breakdown = pred.breakdown,
              pred.xgb = pred.xgb))
}

show_xgboost_example <- function(xgb, explainer, test, idx_to_get) {
  form = which(names(train) == "form") # class column number
  showWaterfall(xgb$model, explainer, xgb$test.data, data.matrix(test[,-form]),
                idx_to_get, type = "binary")
}

# DATA PREP ---------------------------------------------------------------
raw_data <- read_csv("data/input/combined-other.csv") %>%
  mutate(age_range = case_when(
    age < 12 ~ 12, 
    age >= 12 & age < 24 ~ 24, 
    age >= 24 & age < 36 ~ 36, 
    age >= 36 & age < 48 ~ 48, 
    age >= 48 & age < 60 ~ 60, 
    age >= 60 & age < 72 ~ 72, 
    age >= 72  ~ 84)) %>%
  select(form, rarity, complexity_ratings, num_tokens, verbs, 
         pitch_mean, pitch_range, rate, age_range, pair) %>%
  mutate(form = ifelse(form == "CDS", 0, 1)) %>%
  na.omit()

raw_data <- as.data.frame(raw_data)
ages <- c(12, 24, 36, 48, 60, 72, 84)
set.seed(42)

pairs <- read_csv("data-prep/overall/item-info.csv") %>%
  pull(pair) %>%
  unique()



# RUN OVERALL XGBOOST MODEL -----------------------------------------------
dat <- raw_data

split <- initial_split(dat, strata = form, prop = 0.9)
train <- training(split)
test <- testing(split)
cv <- createFolds(train[,"form"], k = 5)

create_xgboost_model(train, test, cv, name = "overall")


# RUN XGBOOST MODELS ON AGE SUBSETS ---------------------------------------
for (i in ages) {
  
  dat <- raw_data %>%
    filter(age_range == i) %>%
    select(-age_range, -pair)
  
  split <- initial_split(dat, strata = form, prop = 0.9)
  train <- training(split)
  test <- testing(split)
  cv <- createFolds(train[,"form"], k = 5)
  
  create_xgboost_model(train, test, cv, name = i)
}

# RUN XGBOOST MODELS ON AGE SUBSETS ---------------------------------------
for (j in pairs) {
  
  dat <- raw_data %>%
    filter(pair == j) %>%
    select(-age_range, -pair)
  
  split <- initial_split(dat, strata = form, prop = 0.9)
  train <- training(split)
  test <- testing(split)
  cv <- createFolds(train[,"form"], k = 5)
  
  create_xgboost_model(train, test, cv, name = j)
}
