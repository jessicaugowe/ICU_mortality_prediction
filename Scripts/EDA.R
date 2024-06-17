
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(corrplot)

# Load Data ---------------------------------------------------------------

df_cleaned <- readRDS("data/cleaned_project_data.rds")

View(df_cleaned)

sum(is.na(df_cleaned)) # Find how many NAs are in the data

colSums(is.na(df_cleaned)) # Find how many NAs are for each variable in the data

# Dataset summary ---------------------------------------------------------

summary(df_cleaned) 

# Prepare data ------------------------------------------------------------

df <- df_cleaned %>% 
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag)) # Ensure target variable is a class (binary)

# Target variable distribution
df %>% 
  ggplot(aes(x = hospital_expire_flag, fill = hospital_expire_flag)) +
  geom_bar()

table(df$hospital_expire_flag)
table(df$hospital_expire_flag)/nrow(df)

# Reshape data for distribution plots: Keep only target + numerical features
plot_data_num <- df %>%
  select(hospital_expire_flag, age, wbc, bmi) %>% 
  pivot_longer(cols = -hospital_expire_flag, names_to = "key", values_to  = "value")

# Feature Distribution plots 
plot_data_num %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")

# Feature Distribution plots by hospital_expire_flag
plot_data_num %>% 
  ggplot(aes(x = value, fill = hospital_expire_flag)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ key, scales = "free")

# Feature Box plots
plot_data_num %>% 
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Feature Box plots by hospital_expire_flag
plot_data_num %>% 
  ggplot(aes(y = value, fill = hospital_expire_flag)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Bar plots 
#gender
df %>%
  group_by(hospital_expire_flag, gender) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(total = sum(n),
         freq = n/total) %>% 
  ungroup() %>% 
  ggplot(aes(x = gender, y = freq)) +
  geom_bar(aes(fill = hospital_expire_flag), stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(freq, accuracy = 0.1), group = hospital_expire_flag), 
            stat = "identity", position = position_dodge(width = 1), vjust = -0.5) +
  labs(y = "Percent")

#hypertension
df %>%
  group_by(hospital_expire_flag, hypertension) %>%
  summarise(n = n(), .groups = 'drop') %>%  # Ensures groups are dropped after summarise
  group_by(hypertension) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ungroup() %>%
  ggplot(aes(x = factor(hypertension), y = freq, fill = factor(hospital_expire_flag))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(freq, accuracy = 0.1), group = factor(hospital_expire_flag)),
            stat = "identity", position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(y = "Percentage", x = "Hypertension Status", fill = "Hospital Expire Flag") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), 
                    labels = c("Survived", "Expired")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of x-axis labels


#chronic_kidney_disease
df %>%
  group_by(hospital_expire_flag, chronic_kidney_disease) %>%
  summarise(n = n(), .groups = 'drop') %>%  # Ensures groups are dropped after summarise
  group_by(chronic_kidney_disease) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ungroup() %>%
  ggplot(aes(x = factor(chronic_kidney_disease), y = freq, fill = factor(hospital_expire_flag))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(freq, accuracy = 0.1), group = factor(hospital_expire_flag)),
            stat = "identity", position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(y = "Percentage", x = "Chronic Kidney Status", fill = "Hospital Expire Flag") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), 
                    labels = c("Survived", "Expired")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of x-axis labels


#cancer
df %>%
  group_by(hospital_expire_flag, cancer) %>%
  summarise(n = n(), .groups = 'drop') %>%  # Ensures groups are dropped after summarise
  group_by(cancer) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ungroup() %>%
  ggplot(aes(x = factor(cancer), y = freq, fill = factor(hospital_expire_flag))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(freq, accuracy = 0.1), group = factor(hospital_expire_flag)),
            stat = "identity", position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(y = "Percentage", x = "Cancer Status", fill = "Hospital Expire Flag") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), 
                    labels = c("Survived", "Expired")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of x-axis labels




