# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(tidymodels)  # For modeling and validation
library(xgboost)    # For XGBoost model
library(pROC)       # For AUC and ROC calculation
library(dplyr)
library(yardstick)

# Load Data -------------------------------------------------------------------
df_cleaned <- readRDS("data/cleaned_project_data.rds")

# Prepare data ----------------------------------------------------------------
df <- df_cleaned %>%
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag), 
         gender = as.factor(gender)) # Ensure target variable is a class (binary)

# Split data--------------------------------------------------------------------
set.seed(42)
split <- initial_split(df, prop = 0.8, strata = hospital_expire_flag)
train_data <- training(split)
test_data <- testing(split)

# Convert categorical variables to numeric in training and test sets-----------
train_data <- train_data %>%
  mutate(gender = as.numeric(as.factor(gender)) - 1,
         race = as.numeric(as.factor(race)))

test_data <- test_data %>%
  mutate(gender = as.numeric(as.factor(gender)) - 1,
         race = as.numeric(as.factor(race)))

# Prepare the data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(train_data, -hospital_expire_flag)),
                      label = as.numeric(as.factor(train_data$hospital_expire_flag)) - 1)

# Define parameters for the XGBoost training-----------------------------------
scale_pos_weight <- sum(train_data$hospital_expire_flag == "0") / sum(train_data$hospital_expire_flag == "1")
xgb_spec <- boost_tree(
  trees = 100,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Define the tuning grid -------------------------------------------------------
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_data),
  size = 30
)

# Create a recipe for preprocessing -------------------------------------------
xgb_recipe <- recipe(hospital_expire_flag ~ ., data = train_data)

# Create a workflow -----------------------------------------------------------
xgb_wflow <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(xgb_recipe)

# Tune the model using cross-validation-----------------------------------------
set.seed(42)
xgb_res <- tune_grid(
  xgb_wflow,
  resamples = vfold_cv(train_data, v = 5, strata = hospital_expire_flag),
  grid = xgb_grid,
  metrics = metric_set(roc_auc, accuracy, f_meas)
)

# Select the best parameters --------------------------------------------------
best_xgb <- select_best(xgb_res, metric = "roc_auc")

# Finalize the workflow using the best parameters------------------------------
final_xgb_wflow <- finalize_workflow(xgb_wflow, best_xgb)

# Fit the final model on the full training dataset------------------------------
fit_final <- fit(final_xgb_wflow, data = train_data)

#Extract the trained xgboost model---------------------------------------------
xgb_model <- extract_fit_engine(fit_final)

# Predict and evaluate on the test data----------------------------------------
predictions <- predict(fit_final, test_data, type = "prob")
roc_info <- roc(test_data$hospital_expire_flag, predictions$.pred_1)
auc_value <- auc(roc_info)
print(paste("AUC:", auc_value))

# Plot the ROC curve-----------------------------------------------------------
plot(roc_info, main = "ROC Curve")

# Save the AUC value ----------------------------------------------------------
write.csv(data.frame(Model = "XGBoost", AUC = auc_value), file = "auc_xgboost.csv", row.names = FALSE)

# Convert predicted probabilities to binary predictions ------------------------
binary_predictions <- ifelse(predictions$.pred_1 > 0.2, 1, 0)

# Create a data frame with actual and predicted labels-------------------------
results <- tibble(
  truth = factor(test_data$hospital_expire_flag, levels = c("0", "1")),  # Ensure the truth is a factor with correct levels
  estimate = factor(binary_predictions, levels = c("0", "1"))  # Convert predictions to a factor with correct levels
)
# Calculate confusion matrix with event level specified -----------------------
conf_matrix <- conf_mat(results, truth = truth, estimate = estimate)
print(conf_matrix)

# Calculate metrics -----------------------------------------------------------
metrics <- metric_set(sens, spec)
metric_results <- metrics(results, truth = truth, estimate = estimate, event_level = "second")

# Print the results
print(metric_results)

#Important predicting features ------------------------------------------------
xgb_importance <- xgb.importance(model = xgb_model)
top_xgb_features <- head(xgb_importance, 3)
print(top_xgb_features)
