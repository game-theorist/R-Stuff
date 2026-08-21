#Plasma Etching

library(tidyverse)

#Tibble making

sample_data <- tribble(~"treatment",
                       ~"values",
                       ~"values",
                       ~"values",
                       ~"values",
                       ~"values",
                       160,
                       575,
                       542,
                       530,
                       539,
                       570,
                       180,
                       565,
                       593,
                       590,
                       579,
                       610,
                       200,
                       600,
                       651,
                       610,
                       637,
                       629,
                       220,
                       725,
                       700,
                       715,
                       685,
                       710
)

sample_data

raw_tribble <- sample_data |> 
  pivot_longer(cols = "values",
               names_to = NULL,
               values_to = "observed_value"
  )|> 
  mutate(treatment = as.factor(`treatment`), .before = 1)


#ANOVA summary

n <- raw_tribble |> 
  summarize(
    n = n()
  ) |> 
  pull()

levels <- length(unique(raw_tribble$treatment))

total_mean <- raw_tribble |>
  summarize(
    total_mean = mean(observed_value)) |> 
  pull()

sample_tribble <- raw_tribble |>
  group_by(treatment) |> 
  mutate(
    treatment_mean = mean(observed_value),
    treatment_n = n(),
    treatment_sd = sd(observed_value),
    treatment_variance = var(observed_value),
    treatment_median = median(observed_value)) |> 
  ungroup()

summarized_tribble <- sample_tribble |>
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
  sum_squares_total <- sum((sample_tribble$observed_value - total_mean) ^ 2)
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

residuals <- sample_tribble |> 
  mutate(residual = observed_value - treatment_mean) |> 
  mutate(scaled_residual = residual / sqrt(mean_squares_error)) |>
  mutate(levene_deviation = abs(observed_value - treatment_median)) |> 
  select(observed_value, residual, scaled_residual, levene_deviation)
