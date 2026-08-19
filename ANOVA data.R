#Tibble making

library(tidyverse)
library(labelled)

sample_data <- tribble(~"treatment", ~"values", ~"values", ~"values", ~"values", ~"values", ~"values",
                       1,
                       4.93(0.05),
                       4.86(0.04),
                       4.75(0.05),
                       4.95(0.06),
                       4.79(0.03),
                       4.88(0.05),
                       2
                       4.85(0.04)
                       4.91(0.02)
                       4.79(0.03)
                       4.85(0.05)
                       4.75(0.03)
                       4.85(0.02)
                       3
                       4.83(0.09)
                       4.88(0.13)
                       4.90(0.11)
                       4.75(0.15)
                       4.82(0.08)
                       4.90(0.12)
                       4
                       4.89(0.03)
                       4.77(0.04)
                       4.94(0.05)
                       4.86(0.05)
                       4.79(0.03)
                       4.76(0.02)
) 

sample_data

raw_tribble <- sample_data |> 
  pivot_longer(cols = "values",
               names_to = NULL,
               values_to = "observed_value"
  )|> 
  mutate(treatment = as.factor(treatment), .before = 1) |> 
  set_variable_labels(
    treatment = "Display Design",
    observed_value = "Percent Increase in Sales"
    
  )


#ANOVA summary

n <- raw_tribble |> 
  summarize(
    m = n()
  ) |> 
  pull()

levels <- length(unique(raw_tribble$treatment))

total_mean <- raw_tribble |>
  summarize(
    total_mean = mean(observed_value)) |> 
  pull()

summarized_tribble <- raw_tribble |>
  group_by(treatment) |> 
  summarize(
    treatment_mean = mean(observed_value),
    treatment_n = n(),
    treatment_sd = sd(observed_value),
    treatment_variance = var(observed_value),
    treatment_median = median(observed_value))
  

summarized_tribble

# Balanced vs Unbalanced Data

balanced_data <- length(unique(summarized_tribble$treatment_n)) == 1

# Sum of squares calculation

if(balanced_data) {
  sum_squares_treatment <- (n / levels) * (sum(((summarized_tribble$treatment_mean - total_mean) ^ 2)))
  sum_squares_total <- sum((raw_tribble$observed_value - total_mean) ^ 2)
  sum_squares_error <- sum_squares_total - sum_squares_treatment
#this part needs correction
  } else {
  sum_squares_treatment <- (sum((summarized_tribble$treatment_mean ^ 2) / summarized_tribble$treatment_n - (total_mean ^ 2) / n))
  sum_squares_total <- sum((response - total_mean) ^ 2)
  sum_squares_error <- sum_squares_total - sum_squares_treatment
}

df_treatment <- levels - 1

df_error <- n - levels

mean_squares_treatment <- sum_squares_treatment / df_treatment

mean_squares_error <- sum_squares_error / df_error

f_statistic <- mean_squares_treatment / mean_squares_error

#Residuals

residuals <- raw_tribble |> 
  mutate(residual = observed_value - treatment_mean) |> 
  mutate(scaled_residual = residual / sqrt(mean_squares_error)) |>
  mutate(levene_deviation = abs(observed_value - treatment_median)) |> 
  select(treatment, observed_value, residual, scaled_residual, levene_deviation)
