library(caret)
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)

data <- read_csv("data/input/combined-other.csv") %>%
  select(form, rarity, complexity_ratings, num_tokens, verbs, 
         pitch_mean, pitch_range, rate, age) %>%
  mutate(form = as.factor(form)) %>%
  na.omit()

# create data split
set.seed(123)
split <- initial_split(data, strata = form, prop = 0.9)
train <- training(split)
test <- testing(split)

# set up default model with hyperparameters to be tuned
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(), 
  sample_size = tune(), 
  mtry = tune(), 
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# set up different combinations of hyperparameters
xgb_grid <- grid_latin_hypercube(
  tree_depth(), 
  min_n(), 
  loss_reduction(), 
  sample_size = sample_prop(), 
  finalize(mtry(), train),  
  learn_rate(), 
  size = 20
)

# add workflow
xgb_wf <- workflow() %>%
  add_formula(form ~ .) %>%
  add_model(xgb_spec)

# create training folds
set.seed(123)
folds <- vfold_cv(train, strata = form)

set.seed(234)
xgb_pred <- tune_grid(
  xgb_wf, 
  resamples = folds, 
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

# visualize hyperparameter space
xgb_pred %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size, 
               names_to = "parameter", 
               values_to = "value") %>%
  ggplot(aes(value, mean, color = parameter)) + 
  geom_point(show.legend = FALSE) + 
  labs(y = "AUC") +
  facet_wrap(.~parameter, scales = "free_x")

show_best(xgb_pred, "roc_auc")

# save best combination of hyperparameters
best_auc <- select_best(xgb_pred, "roc_auc")
final_xgb <- finalize_workflow(xgb_wf, best_auc)

# visualize variable importance
final_xgb %>%
  fit(data = train) %>%
  pull_workflow_fit() %>%
  vip(geom = "col",
      mapping = aes_string(color = "Variable", fill = "Variable"), 
      aesthetics = list(alpha = 0.7)) + 
  scale_x_discrete(breaks = c("pitch_range", "complexity_ratings", 
                              "num_tokens", "rate", "pitch_mean", 
                              "rarity", "verbs", "age"), 
                   labels = c("Pitch range", "Complexity", "Length", "Rate", 
                              "Pitch mean", "Rarity", "Verbs", "Age")) +
  scale_color_manual(values = c("pitch_range" = "#1C9E78", 
                                "complexity_ratings" = "#D95F06", 
                                "num_tokens" = "#7570B4", 
                                "rate" = "#1C9E78", 
                                "pitch_mean" = "#1C9E78", 
                                "rarity" = "#D95F06", 
                                "verbs" = "#7570B4", 
                                "age" = "gray")) +
  scale_fill_manual(values = c("pitch_range" = "#1C9E78", 
                                "complexity_ratings" = "#D95F06", 
                                "num_tokens" = "#7570B4", 
                                "rate" = "#1C9E78", 
                                "pitch_mean" = "#1C9E78", 
                                "rarity" = "#D95F06", 
                                "verbs" = "#7570B4", 
                               "age" = "gray")) +
  labs(y = "Variable Importance") +
  theme_classic(base_size = 10) + 
  theme(legend.position = "none")
ggsave("figs/xgboost-vi-with-age.jpg", dpi = 300, width = 4, height = 3)

# store AUC
final <- last_fit(final_xgb, split)
final %>%
  collect_metrics()

auc <- final %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  pull(.estimate) %>%
  round(., 3)

# create confusion matrix
final %>%
  collect_predictions() %>%
  conf_mat(form, .pred_class)
#            Truth
# Prediction ADS CDS
#       ADS  487  356
#       CDS  609 1815

# plot ROC curve
final %>%
  collect_predictions() %>%
  roc_curve(form, .pred_ADS) %>%
  ggplot(aes(x = specificity, y = sensitivity)) + 
  scale_x_reverse() +
  geom_point(size = 0.5, color = "#235789") + 
  geom_abline(intercept = 1, linetype = "dotted") + 
  geom_label(aes(0.25, 0.5, label = paste0("AUC: ", auc)), color = "#235789") +
  labs(x = "Specificity", y = "Sensitivity", slope = 1) + 
  theme_test(base_size = 20)
ggsave("figs/xgboost-auc-with-age.jpg", dpi = 300, width = 5, height = 5)

library(SHAPforxgboost)

# get shapley values
xgb_fit <- extract_fit_parsnip(final)

shap <- shap.prep(
  xgb_model = extract_fit_engine(xgb_fit), 
  X_train = as.matrix(train[,-1]))


shap.plot.summary(shap)

shap.plot.dependence(shap, 
                     x = "pitch_mean",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)

shap.plot.dependence(shap, 
                     x = "pitch_range",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)

shap.plot.dependence(shap, 
                     x = "rate",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)

shap.plot.dependence(shap, 
                     x = "complexity_ratings",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)

shap.plot.dependence(shap, 
                     x = "rarity",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)

shap.plot.dependence(shap, 
                     x = "num_tokens",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)

shap.plot.dependence(shap, 
                     x = "verbs",
                     color_feature = "age", 
                     smooth = FALSE, 
                     add_hist = TRUE)
