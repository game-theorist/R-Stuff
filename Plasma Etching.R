#Plasma Etching


#Tibble making

sample_data <- tribble(~"power",
                       ~"etch_rate",
                       ~"etch_rate",
                       ~"etch_rate",
                       ~"etch_rate",
                       ~"etch_rate",
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
  pivot_longer(cols = "etch_rate",
               names_to = NULL,
               values_to = "etch_rate"
  )|> 
  mutate(power = as.factor(`power`), .before = 1)


#ANOVA summary

n <- raw_tribble |> 
  summarize(
    n = n()
  ) |> 
  pull()

levels <- length(unique(raw_tribble$power))

total_mean <- raw_tribble |>
  summarize(
    total_mean = mean(etch_rate)) |> 
  pull()

sample_tribble <- raw_tribble |>
  group_by(power) |> 
  mutate(
    treatment_mean = mean(etch_rate),
    treatment_n = n(),
    treatment_sd = sd(etch_rate),
    treatment_variance = var(etch_rate),
    treatment_median = median(etch_rate)) |> 
  ungroup()

summarized_tribble <- sample_tribble |>
  group_by(power) |> 
  summarize(
    treatment_mean = mean(etch_rate),
    treatment_n = n(),
    treatment_sd = sd(etch_rate),
    treatment_variance = var(etch_rate),
    treatment_median = median(etch_rate))


summarized_tribble

# Balanced vs Unbalanced Data

balanced_data <- length(unique(summarized_tribble$treatment_n)) == 1

# Sum of squares calculation

if(balanced_data) {
  sum_squares_treatment <- (n / levels) * (sum(((summarized_tribble$treatment_mean - total_mean) ^ 2)))
  sum_squares_total <- sum((sample_tribble$etch_rate - total_mean) ^ 2)
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
  mutate(residual = etch_rate - treatment_mean) |> 
  mutate(scaled_residual = residual / sqrt(mean_squares_error)) |>
  mutate(levene_deviation = abs(etch_rate - treatment_median)) |> 
  select(etch_rate, residual, scaled_residual, levene_deviation)
