#ANOVA

alpha <- 0.05

side <- "two_sided"

#P-value calculation

lower_tail <- pf(f_statistic, df1 = df_treatment, df2 = df_error, lower.tail = TRUE)

upper_tail <- pf(f_statistic, df1 = df_treatment, df2 = df_error, lower.tail = FALSE)

p_value <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                     side == "less" ~ lower_tail,
                     side == "greater" ~ upper_tail)

#Confidence Interval

degrees_of_freedom <- n - levels

t_critical <- abs(qt(p = alpha / 2, df = degrees_of_freedom))

ci_lower <- summarized_tribble$treatment_mean - t_critical * sqrt(mean_squares_error / summarized_tribble$treatment_n)

ci_upper <- summarized_tribble$treatment_mean + t_critical * sqrt(mean_squares_error / summarized_tribble$treatment_n)

confidence_interval <- tibble(
  treatment = summarized_tribble$power,
  ci_lower = ci_lower,
  ci_upper = ci_upper)

#Results

results <- tibble(
  Source = c("Treatment", "Error", "Total"),
  "Sum of Squares" = c(sum_squares_treatment, sum_squares_error, sum_squares_total),
  "Degrees of Freedom" = c(df_treatment, df_error, NA),
  "Mean Squares" = c(mean_squares_treatment, mean_squares_error, NA),
  "F Statistic" = c(f_statistic, NA, NA),
  "P Value" = c(p_value, NA, NA)
  )

results

confidence_interval

# Contrasts

 ## e.g. mu1 + mu2 = mu3 + mu4 -> C = mean1 + mean2 - mean3 - mean4 -- (yes the double - is correct)

constants <- c(1, 1, -1, -1)

choice <- c(1, 3, 4)

choice_length <- length(choice)

#dont change these []

chosen_constants <- sapply(choice, nth(constants, ~ .x))

chosen_means <- c(nth(summarized_tribble$treatment_mean, choice[1]), nth(summarized_tribble$treatment_mean, choice[2]))

chosen_ns <- c(nth(summarized_tribble$treatment_n, choice[1]), nth(summarized_tribble$treatment_n, choice[2]))
  
contrast <- -diff(chosen_means)

contrast_sum_squares <- contrast ^ 2 / sum((chosen_constants ^ 2) / chosen_ns)
  