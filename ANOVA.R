#ANOVA

library(PMCMRplus) # Dunnett's Test

alpha <- 0.05

side <- "two_sided"

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

tukey_q_critical <- abs(qtukey(tukey_alpha, nmeans = levels, df = df_error, lower.tail = FALSE))

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
  Source = c("Treatment", "Error", "Total"),
  "Sum of Squares" = c(sum_squares_treatment, sum_squares_error, sum_squares_total),
  "Degrees of Freedom" = c(df_treatment, df_error, (df_treatment + df_error)),
  "Mean Squares" = c(mean_squares_treatment, mean_squares_error, NA),
  "F Statistic" = c(f_statistic, NA, NA),
  "P Value" = c(p_value, NA, NA)
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
