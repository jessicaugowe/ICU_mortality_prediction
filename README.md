# Predicting ICU Mortality Using Machine Learning: A Comparative Analysis of Model Performance and Feature Importance

## Overview

This project aims to predict mortality in the Intensive Care Unit (ICU) using the MIMIC IV dataset. The analysis includes data cleaning, exploratory data analysis (EDA), and various predictive modeling techniques such as logistic regression, random forest, and XGBoost. The goal is to develop accurate models that can aid in early identification of high-risk patients and provide insights into the importance of different features.

## Folder Structure and Files

- `cleaned_data.R`: Script for data cleaning and preprocessing. This script processes the raw MIMIC IV data to prepare it for analysis and modeling.
- `EDA.R`: Script for exploratory data analysis. This includes visualizations and statistical summaries to understand the distribution, relationships, and patterns in the data.
- `elect_phenotype.R`: Script for defining and working with the phenotype of interest, specifically mortality in the ICU. This script includes feature engineering and selection tailored to predicting mortality.This selection was also based on past studies.
- `logistic_model.R`: Script for building and evaluating a logistic regression model. This includes model training, validation, and performance metrics.
- `rf_model.R`: Script for building and evaluating a random forest model. This script includes the setup, training, and evaluation of the random forest classifier.
- `xgb_model.R`: Script for building and evaluating an XGBoost model. This includes hyperparameter tuning, training, and assessing the model's performance.
- `table1.R`: Script for generating Table 1, which includes descriptive statistics of the dataset, stratified by Mortality and providing a summary of key variables.
## Setup and Installation

1. **R and RStudio**: Ensure that you have R and RStudio installed on your machine.
2. **Required Packages**: Install the necessary R packages. You can use the following commands to install the required packages:
    ```R
    install.packages(c("dplyr", "ggplot2", "randomForest", "xgboost", "caret", "tableone"))
    ```

## Running the Scripts

1. **Data Cleaning**: Run the `cleaned_data.R` script to preprocess the raw MIMIC IV data.
    ```R
    source("cleaned_data.R")
    ```

2. **Exploratory Data Analysis**: Run the `EDA.R` script to perform EDA.
    ```R
    source("EDA.R")
    ```

3. **Phenotype Engineering**: Run the `elect_phenotype.R` script to define and engineer the phenotype for predicting mortality.
    ```R
    source("elect_phenotype.R")
    ```

4. **Model Building**:
    - Logistic Regression: Run the `logistic_model.R` script.
        ```R
        source("logistic_model.R")
        ```
    - Random Forest: Run the `rf_model.R` script.
        ```R
        source("rf_model.R")
        ```
    - XGBoost: Run the `xgb_model.R` script.
        ```R
        source("xgb_model.R")
        ```

5. **Table Generation**: Run the `table1.R` script to generate descriptive statistics.
    ```R
    source("table1.R")
    ```

## Results



