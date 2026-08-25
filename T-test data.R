#T-test data

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

#Summary

n <- raw_tribble |> 
  summarize(
    n = sum(!is.na(observed_value))
  ) |> 
  pull()

total_mean <- raw_tribble |>
  summarize(
    total_mean = mean(observed_value, na.rm = TRUE)) |> 
  pull()

summarized_tribble <- raw_tribble |>
  group_by(treatment) |> 
  summarize(
    treatment_n = sum(!is.na(observed_value)),
    treatment_mean = mean(observed_value, na.rm = TRUE),
    treatment_median = median(observed_value, na.rm = TRUE),
    treatment_min = min(observed_value),
    treatment_max = max(observed_value),
    treatment_sd = sd(observed_value, na.rm = TRUE),
    treatment_variance = var(observed_value, na.rm = TRUE)
   )
