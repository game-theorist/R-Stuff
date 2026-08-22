#ANOVA data

library(tidyverse)
library(labelled)

#Cleaning tribble

raw_tribble <- sample_data |> 
  pivot_longer(cols = "values",
               names_to = NULL,
               values_to = "observed_value"
  )|> 
  mutate(treatment = as.factor(treatment), .before = 1
         #optional data transformation
         #,observed_value = sqrt(observed_value)
) 

#ANOVA summary

n <- raw_tribble |> 
  summarize(
    n = sum(!is.na(observed_value))
  ) |> 
  pull()

levels <- length(unique(raw_tribble$treatment))

total_mean <- raw_tribble |>
  summarize(
    total_mean = mean(observed_value, na.rm = TRUE)) |> 
  pull()

summarized_tribble <- raw_tribble |>
  group_by(treatment) |> 
  summarize(
    treatment_mean = mean(observed_value, na.rm = TRUE),
    treatment_effect = treatment_mean - total_mean,
    treatment_n = sum(!is.na(observed_value)),
    treatment_sd = sd(observed_value, na.rm = TRUE),
    treatment_variance = var(observed_value, na.rm = TRUE),
    treatment_median = median(observed_value, na.rm = TRUE))
  
summarized_tribble

# Balanced vs Unbalanced Data

balanced_data <- length(unique(summarized_tribble$treatment_n)) == 1

# Sum of squares calculation

if(balanced_data) {
  sum_squares_treatment <- (n / levels) * (sum(((summarized_tribble$treatment_mean - total_mean) ^ 2), na.rm = TRUE))
  sum_squares_total <- sum((raw_tribble$observed_value - total_mean) ^ 2, na.rm = TRUE)
  sum_squares_error <- sum_squares_total - sum_squares_treatment
  } else {
    sum_squares_treatment <- sum(summarized_tribble$treatment_n * (summarized_tribble$treatment_mean - total_mean)^2)
    sum_squares_total <- sum((raw_tribble$observed_value - total_mean) ^ 2, na.rm = TRUE)
    sum_squares_error <- sum_squares_total - sum_squares_treatment
  }

df_treatment <- levels - 1

df_error <- n - levels

mean_squares_treatment <- sum_squares_treatment / df_treatment

mean_squares_error <- sum_squares_error / df_error

f_statistic <- mean_squares_treatment / mean_squares_error

#Residuals

residuals <- raw_tribble |> 
  group_by(treatment) |> 
  mutate(treatment_mean = mean(observed_value, na.rm = TRUE),
         treatment_median = median(observed_value, na.rm = TRUE),
         residual = observed_value - treatment_mean,
         scaled_residual = residual / sqrt(mean_squares_error),
         levene_deviation = abs(observed_value - treatment_median)
  ) |> 
  select(treatment, observed_value, treatment_mean, residual, scaled_residual, levene_deviation)
