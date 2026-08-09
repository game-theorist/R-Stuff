#Tibble making

sample_data <- tribble(~"Estimation Method",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       1,
                       0.34,
                       0.12,
                       1.23,
                       0.70,
                       1.75,
                       0.12,
                       2,
                       0.91,
                       2.94,
                       2.14,
                       2.36,
                       2.86,
                       4.55,
                       3,
                       6.31,
                       8.37,
                       9.75,
                       6.09,
                       9.82,
                       7.24,
                       4,
                       17.15,
                       11.82,
                       10.95,
                       17.20,
                       14.35,
                       16.82
)

sample_data

raw_tribble <- sample_data |> 
  pivot_longer(cols = "Observations",
               names_to = NULL,
               values_to = "measurement"
  )|> 
  mutate(Method = as.factor(`Estimation Method`), .before = 1) |> 
  mutate(`Estimation Method` = NULL)


#ANOVA summary

n <- raw_tribble |> 
  summarize(
    n = n()
  ) |> 
  pull()

levels <- length(unique(sample_tribble$Method))

total_mean <- raw_tribble |>
  summarize(
    total_mean = mean(measurement)) |> 
  pull()

sample_tribble <- raw_tribble |>
  group_by(Method) |> 
  mutate(
    treatment_mean = mean(measurement),
    treatment_n = n(),
    treatment_sd = sd(measurement),
    treatment_variance = var(measurement),
    treatment_median = median(measurement)) |> 
  ungroup()

summarized_tribble <- sample_tribble |>
  group_by(Method) |> 
  summarize(
    treatment_mean = mean(measurement),
    treatment_n = n(),
    treatment_sd = sd(measurement),
    treatment_variance = var(measurement),
    treatment_median = median(measurement))
  

summarized_tribble

# Balanced vs Unbalanced Data

balanced_data <- length(unique(summarized_tribble$treatment_n)) == 1

# Sum of squares calculation

if(balanced_data) {
  sum_squares_treatment <- (n / levels) * (sum(((summarized_tribble$treatment_mean - total_mean) ^ 2)))
  sum_squares_total <- sum((sample_tribble$measurement - total_mean) ^ 2)
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
  mutate(residual = measurement - treatment_mean) |> 
  mutate(scaled_residual = residual / sqrt(mean_squares_error)) |>
  mutate(levene_deviation = abs(measurement - treatment_median)) |> 
  select(measurement, residual, scaled_residual, levene_deviation)
