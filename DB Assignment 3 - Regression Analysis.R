
title: "Assignment 3: Regression Analysis"
output: html_document
date: 11/30/25
name: Diamond Beckom
---

# Install once if needed
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

# Load packages
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)


# Read the Excel file (put the file in your working directory or use full path)
income <- read_excel("C:/Users/diamo/Downloads/income_gender.xlsx")


# Take a quick look
head(income)
str(income)
# Convert weekly income columns to numeric
income <- income %>%
  mutate(
    All_weekly_num = as.numeric(gsub(",", "", All_weekly)),
    M_weekly_num   = as.numeric(gsub(",", "", M_weekly)),
    F_weekly_num   = as.numeric(gsub(",", "", F_weekly)),
    prop_female    = F_workers / All_workers
  )


# Drop rows with missing or zero workers (to avoid weird values)
income_clean <- income %>%
  filter(!is.na(All_weekly_num),
         !is.na(prop_female),
         All_workers > 0)

# Check results
summary(income_clean$All_weekly_num)
summary(income_clean$prop_female)
nrow(income_clean)

# Overall summary
summary(income_clean[, c("All_weekly_num", "M_weekly_num", "F_weekly_num", "prop_female")])

# Histogram of overall weekly income
ggplot(income_clean, aes(x = All_weekly_num)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Overall Weekly Income",
       x = "All Weekly Income",
       y = "Count")

# Histogram of proportion female
ggplot(income_clean, aes(x = prop_female)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Proportion Female by Occupation",
       x = "Proportion Female",
       y = "Count")

ggplot(income_clean, aes(x = prop_female, y = All_weekly_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Weekly Income vs. Proportion Female",
       x = "Proportion of Women in Occupation",
       y = "Overall Weekly Income")

model1 <- lm(All_weekly_num ~ prop_female, data = income_clean)

# View model summary
summary(model1)

# Basic diagnostic plots
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# New data for prediction
new_data <- data.frame(
  prop_female = c(0.2, 0.5, 0.8)
)

# Predicted weekly income with 95% prediction intervals
predictions <- predict(model1, newdata = new_data, interval = "prediction")
predictions
