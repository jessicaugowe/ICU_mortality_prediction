library(dplyr)
library(readr)
library(tableone)
library(tidyverse)
library(dplyr)


# Load Data -------------------------------------------------------------------
df_cleaned <- readRDS("data/cleaned_project_data.rds")

#view(df_cleaned) uncomment to view the data

# Prepare data ----------------------------------------------------------------
df_mutate <- df_cleaned %>%
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag), gender = as.factor(gender)) # Ensure target variable is a class (binary)

#Mutate columns ----------------------------------------------------------------
df_final <- df_mutate %>%
  mutate(
    # Correctly recode gender
    gender = factor(ifelse(gender == "M" | gender == "1", "Male", ifelse(gender == "F" | gender == "0", "Female", NA)), levels = c("Female", "Male")),
    hypertension = factor(hypertension, levels = c("0", "1"), labels = c("No", "Yes")),
    chronic_kidney_disease = factor(chronic_kidney_disease, levels = c("0", "1"), labels = c("No", "Yes")),
    cancer = factor(cancer, levels = c("0", "1"), labels = c("No", "Yes")),
    race = factor(race, levels = unique(race))  # Make sure all levels are included and order if necessary
  )

# Improve variable names and groupings ------------------------------------
df_improve <- df_final %>%
  rename(
    Gender = gender,
    Age = age,
    Race = race,
    "Heart rate" =  heart_rate,
    "White blood cell" = wbc,
    Hypertension = hypertension,
    "Chronic kidney disease" = chronic_kidney_disease,
    Cancer = cancer,
    Mortality = hospital_expire_flag,
    BMI = bmi
  )

df_race <- df_improve %>%
  mutate(
    Race = case_when(
      Race %in% c("WHITE", "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN", "WHITE - OTHER EUROPEAN", "WHITE - RUSSIAN") ~ 'White',
      Race %in% c("BLACK/AFRICAN", "BLACK/AFRICAN AMERICAN", "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND") ~ 'Black/African American',
      Race %in% c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CHINESE", "ASIAN - KOREAN", "ASIAN - SOUTH EAST ASIAN") ~ 'Asian',
      Race %in% c("HISPANIC OR LATINO", "HISPANIC/LATINO - CENTRAL AMERICAN", "HISPANIC/LATINO - COLUMBIAN", "HISPANIC/LATINO - CUBAN", 
                  "HISPANIC/LATINO - DOMINICAN", "HISPANIC/LATINO - GUATEMALAN", "HISPANIC/LATINO - HONDURAN", 
                  "HISPANIC/LATINO - MEXICAN", "HISPANIC/LATINO - PUERTO RICAN", "HISPANIC/LATINO - SALVADORAN") ~ 'Hispanic/Latino',
      TRUE ~ 'Other/Unknown'
    )
  )

# Exploratory Data Analysis -----------------------------------------------
summary(df_race)
sum(is.na(df_race)) # Total missing values
colSums(is.na(df_race))  # Missing values per column

# Variables for TableOne ---------------------------------------------------
vars <- c("Age", "Gender", "Race", "BMI", "Heart rate", "White blood cell", "Hypertension", "Chronic kidney disease", "Cancer")
cat_vars <- c("Gender", "Race", "Hypertension", "Chronic kidney disease", "Cancer")
var_non_norm <- c("Age", "BMI", "Heart rate", "White blood cell")

# Create Table 1 using TableOne with p-values and stratification by Mortality --
table1_created <- CreateTableOne(vars = vars, strata = "Mortality", data = df_race, factorVars = cat_vars, test = TRUE)

table_overall <- CreateTableOne(vars = vars, data = df_race, factorVars = cat_vars, test = TRUE)
print(table_overall)
# Print the table with proper formatting -----------------------------------
table1_for_export <- print(table1_created, nonnormal = var_non_norm, quote = FALSE, noSpaces = TRUE, pDigits = 3)
table_overall_export <- print(table_overall, nonnormal = var_non_norm, quote = FALSE, noSpaces = TRUE, pDigits = 3)
# Clean up row names
rownames(table1_for_export) <- gsub('1_', '', rownames(table1_for_export))
rownames(table1_for_export) <- gsub('2_', '', rownames(table1_for_export))
rownames(table1_for_export) <- gsub('3_', '', rownames(table1_for_export))
rownames(table1_for_export) <- gsub('4_', '', rownames(table1_for_export))
rownames(table1_for_export) <- gsub('5_', '', rownames(table1_for_export))
rownames(table1_for_export) <- gsub('_', ' ', rownames(table1_for_export))


# Extract the table with formatting
table1_printed <- print(table1_created, nonnormal = var_non_norm, quote = FALSE, noSpaces = TRUE, pDigits = 3)

# Convert the output to a character matrix
table1_matrix <- as.matrix(table1_printed)

# Create a new data frame for the cleaned and structured output
cleaned_table <- data.frame(
  Measure = rownames(table1_matrix),
  Group0 = table1_matrix[, 1],
  Group1 = table1_matrix[, 2],
  pValue = table1_matrix[, 3]
)

# Function to add asterisks to p-values based on significance
add_asterisks <- function(p) {
  if (is.na(p) || p == "") return("")
  num_p <- as.numeric(sub("<", "", p))
  if (num_p < 0.001) return("***")
  else if (num_p < 0.01) return("**")
  else if (num_p < 0.05) return("*")
  else return("")
}

# Apply the function to add asterisks to p-values
cleaned_table$pValue <- sapply(cleaned_table$pValue, function(x) {
  paste0(x, add_asterisks(x))
})

install.packages("openxlsx")
  library(openxlsx)
# Write the data to an Excel file
write.xlsx(cleaned_table, file = "Table1_Export.xlsx", rowNames = FALSE)

# Print to check output
print(cleaned_table)

# Remove redundant "Yes" from the category names
table1_for_export <- gsub(" = Yes", "", table1_for_export)
print(table1_for_export)


