#ANOVA data (RCBD)

library(tidyverse)
library(labelled)
library(PMCMRplus) # Dunnett's Test

alpha <- 0.05

side <- "two_sided"

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

treatment_levels <- length(unique(raw_tribble$treatment))

block_levels <- length(unique(raw_tribble$block))

grand_total <- raw_tribble |>
  summarize(
    grand_total = sum(observed_value, na.rm = TRUE)) |> 
  pull()

grand_mean <- raw_tribble |>
  summarize(
    grand_mean = mean(observed_value, na.rm = TRUE)) |> 
  pull()

treatment_totals <- raw_tribble |>
  arrange(treatment) |> 
  summarize(
    treatment_total = sum(observed_value, na.rm = TRUE),
    .by = treatment
  ) |> 
  pull()

treatment_means <- raw_tribble |>
  arrange(treatment) |> 
  summarize(
    treatment_mean = mean(observed_value, na.rm = TRUE),
    .by = treatment
  ) |> 
  pull()

block_totals <- raw_tribble |>
  arrange(block) |> 
  summarize(
    block_total = sum(observed_value, na.rm = TRUE),
    .by = block
  ) |> 
  pull()

block_means <- raw_tribble |>
  arrange(block) |> 
  summarize(
    block_mean = mean(observed_value, na.rm = TRUE),
    .by = block
  ) |> 
  pull()

# Sum of squares calculation

sum_squares_treatment <- ((sum(treatment_totals ^ 2)) / block_levels) - ((grand_total ^ 2) / n)

sum_squares_block <- ((sum(block_totals ^ 2)) / treatment_levels) - ((grand_total ^ 2) / n)

sum_squares_total <- (sum(raw_tribble$observed_value ^ 2)) - ((grand_total ^ 2) / n)

sum_squares_error <- sum_squares_total - sum_squares_treatment - sum_squares_blocks

#Degrees of freedom

df_treatment <- treatment_levels - 1

df_block <- block_levels - 1

df_error <- (treatment_levels - 1) * (block_levels - 1)

df_total <- n - 1

#Mean squares

mean_squares_treatment <- sum_squares_treatment / df_treatment

mean_squares_block <- sum_squares_block / df_block

mean_squares_error <- sum_squares_error / df_error

#F-statistic

f_statistic <- mean_squares_treatment / mean_squares_error

#Residuals

residuals <- raw_tribble |>
  mutate(treatment_mean = mean(observed_value, na.rm = TRUE),
         treatment_effect = treatment_mean - grand_mean,
         .by = treatment
  ) |> 
  mutate(block_mean = mean(observed_value, na.rm = TRUE),
         block_effect = block_mean - grand_mean,
         .by = block
  ) |> 
  mutate(predicted_value = grand_mean + treatment_effect + block_effect,
         residual = observed_value - predicted_value,
         
  ) |> 
  select(treatment, treatment_mean, block, block_mean, observed_value, predicted_value, residual)

#Checking normality of residuals

library(qqplotr)
library(patchwork)

plot_residuals <- residuals |> 
  ggplot(aes(sample = residual)) +
  stat_qq_point() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_scaled_residuals <- residuals |> 
  ggplot(aes(sample = residual)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_residuals_versus_predicted <- residuals |> 
  ggplot(aes(x = predicted_value, y = residual)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

plot_residuals_versus_treatment <- residuals |> 
  ggplot(aes(x = treatment, y = residual)) +
  geom_jitter(width = 0) +
  geom_hline(yintercept = 0, color = "blue", linewidth = 1)

plot_residuals_versus_block <- residuals |> 
  ggplot(aes(x = block, y = residual)) +
  geom_jitter(width = 0) +
  geom_hline(yintercept = 0, color = "blue", linewidth = 1)

plot_residuals + plot_scaled_residuals + plot_residuals_versus_predicted + plot_residuals_versus_treatment + plot_residuals_versus_block

#P-value calculation

p_value <- pf(f_statistic, df1 = df_treatment, df2 = df_error, lower.tail = FALSE)

#Confidence Interval

degrees_of_freedom <- n - levels

t_critical <- abs(qt(p = alpha / 2, df = degrees_of_freedom))

treatment_mean_ci_lower <- summarized_tribble$treatment_mean - t_critical * sqrt(mean_squares_error / summarized_tribble$treatment_n)

treatment_mean_ci_upper <- summarized_tribble$treatment_mean + t_critical * sqrt(mean_squares_error / summarized_tribble$treatment_n)

confidence_interval <- summarized_tribble |>
  mutate(
    treatment_mean_lower = treatment_mean - t_critical * sqrt(mean_squares_error / treatment_n),
    treatment_mean_upper = treatment_mean + t_critical * sqrt(mean_squares_error / treatment_n)
  ) |> 
  select(treatment, treatment_mean, treatment_mean_lower, treatment_mean_upper, treatment_effect)

#Tukey's HSD

tukey_alpha <- 0.05

tukey_q_critical <- abs(qtukey(tukey_alpha, nmeans = block_levels, df = df_error, lower.tail = FALSE))

## differences

tukey_differences <- summarized_tribble |> 
  cross_join(summarized_tribble, suffix = c("_1", "_2")) |> 
  filter(as.integer(treatment_1) < as.integer(treatment_2)) |>
  mutate(tukey_statistic = tukey_q_critical * (sqrt((mean_squares_error / 2) * ((1 / treatment_n_1) + 1 / treatment_n_2))),
         difference = treatment_mean_1 - treatment_mean_2,
         conf_lower = difference - tukey_statistic,
         conf_upper = difference + tukey_statistic,
         significant = if_else(abs(difference) > tukey_statistic, "Yes", "No")
  ) |>  
  
  select(treatment_1, treatment_2, tukey_statistic, difference, conf_lower, conf_upper, significant)

#Fisher Least Significant Difference (LSD)

lsd_alpha <- 0.05

lsd_differences <- summarized_tribble |> 
  cross_join(summarized_tribble, suffix = c("_1", "_2")) |> 
  filter(as.integer(treatment_1) < as.integer(treatment_2)) |> 
  mutate(difference = treatment_mean_1 - treatment_mean_2,
         lsd = abs(qt(lsd_alpha / 2, df_error, lower.tail = FALSE) * sqrt(mean_squares_error * ((1 / treatment_n_1) + (1 / treatment_n_2)))),
         lsd_t = difference / sqrt(mean_squares_error * ((1 / treatment_n_1) + (1 / treatment_n_2))),
         conf_lower = difference - lsd,
         conf_upper = difference + lsd,
         lower_tail = pt(lsd_t, df = df_error, lower.tail = TRUE),
         upper_tail = pt(lsd_t, df = df_error, lower.tail = FALSE),
         lsd_p_value = 2 * pmin(lower_tail, upper_tail)
  ) |> 
  select(treatment_1, treatment_2, lsd, difference, conf_lower, conf_upper, lsd_t, lsd_p_value)

#Results

results <- tibble(
  Source = c("Treatment", "Block", "Error", "Total"),
  "Sum of Squares" = c(sum_squares_treatment, sum_squares_block, sum_squares_error, sum_squares_total),
  "Degrees of Freedom" = c(df_treatment, df_block, df_error, df_total),
  "Mean Squares" = c(mean_squares_treatment, mean_squares_block, mean_squares_error, NA),
  "F Statistic" = c(f_statistic, NA, NA, NA),
  "P Value" = c(p_value, NA, NA, NA)
)

results

confidence_interval

tukey_differences

lsd_differences

dunnett_differences

#Dunnett's Test

control_treatment <- 220

dunnett_control <- summarized_tribble |>
  filter(treatment == control_treatment)

dunnett_non_control <- summarized_tribble |> 
  filter(treatment != control_treatment)

dunnett_critical <- qDunnett(alpha / 2, n0 = dunnett_control$treatment_n, n = dunnett_non_control$treatment_n)

dunnett_differences <- dunnett_non_control |> 
  mutate(control_difference = treatment_mean - dunnett_control$treatment_mean,
         critical_difference = dunnett_critical * sqrt(mean_squares_error * ((1 / treatment_n) + (1 / dunnett_non_control$treatment_n)))
  )

