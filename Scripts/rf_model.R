# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(pROC)
library(dials)  # For parameter functions
library(yardstick)
library(themis)
library(caret)
library(dplyr)
library(readr)


# Load Data -------------------------------------------------------------------
df_cleaned <- readRDS("data/cleaned_project_data.rds")

#view(df_cleaned) uncomment to view the data


# Prepare data ----------------------------------------------------------------
df <- df_cleaned %>%
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag), gender = as.factor(gender)) # Ensure target variable is a class (binary)

#Split data
set.seed(42)
split <- initial_split(df, prop = 0.8, strata = hospital_expire_flag)
train_data <- training(split)
test_data <- testing(split)

# Create 5-fold cross-validation folds
train_folds <- vfold_cv(train_data, v = 5, strata = hospital_expire_flag)


# Adjusting class weights directly in the Random Forest model
# Ensuring that the model parameters are set to be tuned
rf_model <- rand_forest(
  trees = tune(),    # Specify number of trees to tune
  mtry = tune(),     # Specify mtry to tune
  min_n = tune()     # Specify min_n to tune
) %>%
  set_engine("randomForest", class.weights = c(`0` = 1, `1` = 10)) %>%
  set_mode("classification")


# Applying up-sampling using SMOTE for the minority class
# Adjusting the recipe to ensure all predictors handled by SMOTE are numeric
df_rec <- recipe(hospital_expire_flag ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_smote(hospital_expire_flag)

# Define the tuning grid
rf_grid <- grid_regular(
  trees(range = c(500, 1500)),   # Specify a range of tree counts to explore
  mtry(range = c(2, 6)),         # Range for the number of variables considered at each split
  min_n(range = c(5, 15)),       # Minimum number of samples in nodes
  levels = 5                     # The number of levels or values per parameter
)

# Create model workflow
rf_wflow <- workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(df_rec)

# Train and tune the model
set.seed(42)
rf_res <- tune_grid(
  rf_wflow,
  resamples = train_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc, accuracy)
)

# Check for model training issues
show_notes(rf_res)

# Assuming you have already defined `rf_res` and `rf_wflow`

# Select the best parameters based on ROC AUC
best_rf_params <- select_best(rf_res, metric = "roc_auc")

# Finalize the workflow using the best parameters found
final_wf <- finalize_workflow(rf_wflow, best_rf_params)

# Fit the final model on the full training dataset
fit_final <- fit(final_wf, data = train_data)

#Extract the trained xgboost model
rf_model <- randomForest(hospital_expire_flag ~ ., data = train_data, classwt = c(0 = 1, 1 = 10), importance = TRUE)

# Evaluate on test data
# Assuming 'fit_final' is your trained model and 'test_data' is prepared
predictions_prob <- predict(fit_final, test_data, type = "prob")
predictions_class <- predict(fit_final, test_data, type = "class")
roc_obj <- roc(test_data$hospital_expire_flag, predictions_prob$.pred_1)

# Combine predictions with the test set
predictions <- test_data %>%
  mutate(
    truth = hospital_expire_flag,
    prob_positive = predictions_prob$.pred_1,
    estimate = predictions_class$.pred_class
  )

# Apply the optimal threshold to classify predictions
adjusted_predictions <- ifelse(predictions$prob_positive > coords$threshold, 1, 0)
predictions <- predictions %>%
  mutate(adjusted_class = as.factor(adjusted_predictions))

# Experiment with different thresholds
thresholds <- seq(0.1, 0.3, by = 0.05)
metrics <- data.frame(threshold = thresholds, specificity = NA, sensitivity = NA)

for (i in seq_along(thresholds)) {
  result <- calculate_metrics(thresholds[i], predictions)
  metrics$specificity[i] <- result$specificity
  metrics$sensitivity[i] <- result$sensitivity
}

print(metrics)

# Choose a threshold that gives you the desired balance between sensitivity and specificity
best_threshold <- 0.15  # Choose threshold that maximizes sensitivity
print(paste("Best threshold for high sensitivity:", best_threshold))

# Apply the best threshold to classify predictions
adjusted_predictions <- ifelse(predictions$prob_positive > best_threshold, 1, 0)
predictions <- predictions %>%
  mutate(adjusted_class = as.factor(adjusted_predictions))

# Calculate confusion matrix using caret
conf_matrix <- confusionMatrix(predictions$adjusted_class, predictions$truth, positive = "1")
print(conf_matrix)

# Calculate Precision and Recall from the confusion matrix
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']

# Optional: Plot ROC curve with adjusted predictions
roc_obj_adjusted <- roc(predictions$truth, predictions$prob_positive)
auc_value_adjusted <- auc(roc_obj_adjusted)
print(paste("ROC AUC with adjusted threshold:", auc_value_adjusted))

# Find top 3 features-----------------------------------------------------------
importance_lr <- summary(rf_model)$coefficients
print(importance_lr)