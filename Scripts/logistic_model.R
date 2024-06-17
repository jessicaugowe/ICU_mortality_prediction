# Load necessary libraries
library(tidyverse)
library(caret)
library(dplyr)
library(pROC)
library(car)

# Load Data -------------------------------------------------------------------
df_cleaned <- readRDS("data/cleaned_project_data.rds")

#view(df_cleaned) uncomment to view the data

# Prepare data ----------------------------------------------------------------
df <- df_cleaned %>%
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag), gender = as.factor(gender)) # Ensure target variable is a class (binary)

set.seed(42)  # For reproducibility
train_index <- createDataPartition(df$hospital_expire_flag, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Model fitting --------------------------------------------------------------
model <- glm(hospital_expire_flag ~ ., data = train_data, family = binomial())

# Prediction ------------------------------------------------------------------
probabilities <- predict(model, test_data, type = "response")

# Define the function to adjust threshold and calculate confusion matrix
adjust_threshold <- function(threshold, probabilities, true_labels) {
  predicted_class <- factor(ifelse(probabilities > threshold, 1, 0), levels = c(0, 1))
  true_labels <- factor(true_labels, levels = c(0, 1))
  conf_matrix <- confusionMatrix(predicted_class, true_labels, positive = "1")
  return(conf_matrix)
}

# Function to print confusion matrix metrics and the confusion matrix itself
print_conf_matrix_metrics <- function(conf_matrix) {
  print(conf_matrix)  # Print the full confusion matrix
  sensitivity <- conf_matrix$byClass['Sensitivity']
  specificity <- conf_matrix$byClass['Specificity']
  balanced_accuracy <- conf_matrix$byClass['Balanced Accuracy']
  print(paste("Sensitivity (Recall):", sensitivity))
  print(paste("Specificity:", specificity))
  print(paste("Balanced Accuracy:", balanced_accuracy))
}

# Find the optimal threshold to improve specificity----------------------------
thresholds <- seq(0.1, 0.5, by = 0.1)
results <- data.frame(Threshold = thresholds, Sensitivity = NA, Specificity = NA, Balanced_Accuracy = NA)

for (i in 1:length(thresholds)) {
  conf_matrix <- adjust_threshold(thresholds[i], probabilities, as.factor(test_data$hospital_expire_flag))
  results$Sensitivity[i] <- conf_matrix$byClass['Sensitivity']
  results$Specificity[i] <- conf_matrix$byClass['Specificity']
  results$Balanced_Accuracy[i] <- conf_matrix$byClass['Balanced Accuracy']
}

# Find the threshold that maximizes balanced accuracy
optimal_threshold <- results$Threshold[which.max(results$Balanced_Accuracy)]
print(paste("Optimal Threshold:", optimal_threshold))

# Apply the optimal threshold and print the confusion matrix metrics
conf_matrix_optimal <- adjust_threshold(optimal_threshold, probabilities, as.factor(test_data$hospital_expire_flag))
print_conf_matrix_metrics(conf_matrix_optimal)

# Plot sensitivity vs. specificity---------------------------------------------
ggplot(results, aes(x = Threshold)) +
  geom_line(aes(y = Sensitivity, color = "Sensitivity")) +
  geom_line(aes(y = Specificity, color = "Specificity")) +
  geom_line(aes(y = Balanced_Accuracy, color = "Balanced Accuracy")) +
  labs(title = "Sensitivity, Specificity, and Balanced Accuracy vs. Threshold",
       x = "Threshold", y = "Metric Value") +
  scale_color_manual(values = c("Sensitivity" = "blue", "Specificity" = "red", "Balanced Accuracy" = "green"))

# Checking for multi-collinearity----------------------------------------------
# Fit the initial model
model <- glm(hospital_expire_flag ~ ., data = train_data, family = binomial())

# Calculate VIF for each predictor
vif_values <- vif(model)

# Print the VIF values
print(vif_values)

# Calculate ROC and AUC---------------------------------------------------------
roc_response <- roc(response = test_data$hospital_expire_flag, predictor = probabilities)
auc_score <- auc(roc_response)
print(paste("AUC:", auc_score))

# Save the AUC value
write.csv(data.frame(Model = "Logistic Regression", AUC = auc_score), file = "auc_logistic_regression.csv", row.names = FALSE)

# Plot ROC curve ---------------------------------------------------------------
plot(roc_response)

# Find top 3 features-----------------------------------------------------------
importance_lr <- summary(model)$coefficients
print(importance_lr)



