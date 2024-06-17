library(dplyr)
library(readr)
library(tableone)
library(tidyverse)
#install.packages("VIM")
library(VIM)

# Load Data
df <- read_csv(file = "data/project.csv")
# View(df) # Uncomment to view the data

# Calculating Age
df$admittime <- as.POSIXct(df$admittime, format="%Y-%m-%d %H:%M:%S")
df$admit_year <- as.integer(format(df$admittime, "%Y"))
df$anchor_year <- as.integer(df$anchor_year)
df$age <- df$anchor_age + (df$admit_year - df$anchor_year)

# Convert all necessary variables to appropriate types
df <- df %>%
  mutate(
    anchor_age = as.numeric(anchor_age),
    admit_year = as.numeric(admit_year),
    anchor_year = as.numeric(anchor_year),
    age = as.numeric(age)
  )

# Checking for missing values -------------------------------------------------
print(sum(is.na(df))) # Find how many NAs are in the data
print(colSums(is.na(df))) # Find how many NAs are for each variable in the data

# Remove columns not needed for prediction -------------------------------------
df <- df %>%
  select(-subject_id, -stay_id, -hadm_id, -anchor_age, -anchor_year, -admittime, -dischtime, -intime, -outtime, -admit_year)

# Imputing missing values
# Filter out unrealistic BMI values
df_filtered <- df %>%
  filter((bmi >= 10 & bmi <= 100) | is.na(bmi))

# Check summary statistics after filtering
print(summary(df_filtered))
print(summary(df_filtered %>% filter(hospital_expire_flag == 0) %>% select(bmi)))
print(summary(df_filtered %>% filter(hospital_expire_flag == 1) %>% select(bmi)))

# Calculate IQR for BMI in each group manually
bmi_mortality_0 <- df_filtered %>% filter(hospital_expire_flag == 0) %>% pull(bmi)
bmi_mortality_1 <- df_filtered %>% filter(hospital_expire_flag == 1) %>% pull(bmi)

iqr_0 <- IQR(bmi_mortality_0, na.rm = TRUE)
iqr_1 <- IQR(bmi_mortality_1, na.rm = TRUE)

print(summary(bmi_mortality_0))
print(summary(bmi_mortality_1))

print(paste("IQR for BMI in mortality group 0:", iqr_0))
print(paste("IQR for BMI in mortality group 1:", iqr_1))

#Solving for missing values----------------------------------------------------
# Perform KNN imputation
df_knn_imputed <- kNN(df, k = 5, imp_var = FALSE)

# Check for any remaining NAs after imputation
sum(is.na(df_knn_imputed))
colSums(is.na(df_knn_imputed))

# Treating outliers for heart rate, bmi and wbc---------------------------------
# Function to cap outliers based on IQR method
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x <- ifelse(x < lower_bound, lower_bound, x)
  x <- ifelse(x > upper_bound, upper_bound, x)
  return(x)
}
View(df_knn_imputed)
# Apply outlier capping to multiple columns ------------------------------------
df_capped <- df_knn_imputed %>%
  mutate(
    heart_rate = cap_outliers(heart_rate),
    wbc = cap_outliers(wbc),
    bmi = cap_outliers(bmi)
  )

# Check the structure of the modified data -------------------------------------
print(str(df_capped))

print(sum(is.na(df_capped))) # Find how many NAs are in the data
print(colSums(is.na(df_capped)))

# Saving the cleaned data----------------------------------------------------------
# Load necessary libraries
library(readr)

# Save the cleaned data as a CSV file
write_csv(df_capped, "data/cleaned_project_data.csv")

# Save the cleaned data as an RDS file
saveRDS(df_capped, "data/cleaned_project_data.rds")
