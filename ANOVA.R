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
  treatment = summarized_tribble$treatment,
  ci_lower = ci_lower,
  ci_upper = ci_upper)

# Contrasts

 ## e.g. mu1 + mu2 = mu3 + mu4 -> C = mean1 + mean2 - mean3 - mean4

## constants for each hypothesis must sum to 0

# Hypotheses example: mu1 = mu2 -> c1 = 1, c2 = -1

# H01: mu1 = mu2

constants_h01 <- c(1, -1, 0, 0)

constrast_h01 <- (sum(constants_h01 * summarized_tribble$treatment_mean))

sum_squares_constrast_h01 <- (constrast_h01 ^ 2) / (sum(constants_h01 ^ 2)/ summarized_tribble$treatment_n[1])

f_constrast_h01 <- sum_squares_constrast_h01 / mean_squares_error

lower_tail <- pf(f_constrast_h01, df1 = 1, df2 = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pf(f_constrast_h01, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)

p_value_constrast_h01 <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                                   side == "less" ~ lower_tail,
                                   side == "greater" ~ upper_tail)

#H02: mu1 + mu2 = mu3 + mu4

constants_h02 <- c(1, 1, -1, -1)

constrast_h02 <- (sum(constants_h02 * summarized_tribble$treatment_mean))

sum_squares_constrast_h02 <- (constrast_h02 ^ 2) / (sum(constants_h02 ^ 2)/ summarized_tribble$treatment_n[1])

f_constrast_h02 <- sum_squares_constrast_h02 / mean_squares_error

lower_tail <- pf(f_constrast_h02, df1 = 1, df2 = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pf(f_constrast_h02, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)

p_value_constrast_h02 <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                                   side == "less" ~ lower_tail,
                                   side == "greater" ~ upper_tail)


#H03: mu3 = mu4

constants_h03 <- c(0, 0, 1, -1)

constrast_h03 <- (sum(constants_h03 * summarized_tribble$treatment_mean))

sum_squares_constrast_h03 <- (constrast_h03 ^ 2) / (sum(constants_h03 ^ 2)/ summarized_tribble$treatment_n[1]) 

f_constrast_h03 <- sum_squares_constrast_h03 / mean_squares_error

lower_tail <- pf(f_constrast_h03, df1 = 1, df2 = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pf(f_constrast_h03, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)

p_value_constrast_h03 <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                                   side == "less" ~ lower_tail,
                                   side == "greater" ~ upper_tail)


#Tukey

#q_statistic <- (max(summarized_tribble$treatment_mean) - min(summarized_tribble$treatment_mean)) / (mean_squares_error / mean(summarized_tribble$treatment_n))

q_critical <- qtukey(alpha, nmeans = levels, df = df_error, lower.tail = FALSE)

tukey_statistic <- q_critical * sqrt(mean_squares_error / mean(summarized_tribble$treatment_n))

 ## differences

differences <- summarized_tribble |> 
  cross_join(summarized_tribble, suffix = c("_1", "_2")) |> 
  filter(as.integer(treatment_1) < as.integer(treatment_2)) |>
  mutate(difference = treatment_mean_1 - treatment_mean_2) |> 
  select(treatment_1, treatment_2, difference)

#Results

results <- tibble(
  Source = c("Treatment", "Ort Contrast 1", "Ort Contrast 2", "Ort Contrast 3", "Error", "Total"),
  "Sum of Squares" = c(sum_squares_treatment, sum_squares_constrast_h01, sum_squares_constrast_h02,
                       sum_squares_constrast_h03, sum_squares_error, sum_squares_total),
  "Degrees of Freedom" = c(df_treatment, 1, 1, 1, df_error, (df_treatment + df_error)),
  "Mean Squares" = c(mean_squares_treatment, sum_squares_constrast_h01, sum_squares_constrast_h02, sum_squares_constrast_h03, mean_squares_error, NA),
  "F Statistic" = c(f_statistic, f_constrast_h01, f_constrast_h02, f_constrast_h03, NA, NA),
  "P Value" = c(p_value, p_value_constrast_h01, p_value_constrast_h02, p_value_constrast_h03, NA, NA)
)

results

confidence_interval
  