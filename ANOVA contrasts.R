# Contrasts

## e.g. mu1 + mu2 = mu3 + mu4 -> C = mean1 + mean2 - mean3 - mean4

## constants for each hypothesis must sum to 0

# Hypotheses example: mu1 = mu2 -> c1 = 1, c2 = -1

# H01: mu1 = mu2

constants_h01 <- c(1, -2, 1)

constrast_h01 <- (sum(constants_h01 * summarized_tribble$treatment_mean))

sum_squares_constrast_h01 <- (constrast_h01 ^ 2) / (sum(constants_h01 ^ 2)/ summarized_tribble$treatment_n[1])

f_constrast_h01 <- sum_squares_constrast_h01 / mean_squares_error

lower_tail <- pf(f_constrast_h01, df1 = 1, df2 = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pf(f_constrast_h01, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)

p_value_constrast_h01 <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                                   side == "less" ~ lower_tail,
                                   side == "greater" ~ upper_tail)

#H02: mu1 + mu2 = mu3 + mu4

constants_h02 <- c(1, 1, 0)

constrast_h02 <- (sum(constants_h02 * summarized_tribble$treatment_mean))

sum_squares_constrast_h02 <- (constrast_h02 ^ 2) / (sum(constants_h02 ^ 2)/ summarized_tribble$treatment_n[1])

f_constrast_h02 <- sum_squares_constrast_h02 / mean_squares_error

lower_tail <- pf(f_constrast_h02, df1 = 1, df2 = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pf(f_constrast_h02, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)

p_value_constrast_h02 <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                                   side == "less" ~ lower_tail,
                                   side == "greater" ~ upper_tail)


#H03: mu3 = mu4

constants_h03 <- c(0, 0, 0)

constrast_h03 <- (sum(constants_h03 * summarized_tribble$treatment_mean))

sum_squares_constrast_h03 <- (constrast_h03 ^ 2) / (sum(constants_h03 ^ 2)/ summarized_tribble$treatment_n[1]) 

f_constrast_h03 <- sum_squares_constrast_h03 / mean_squares_error

lower_tail <- pf(f_constrast_h03, df1 = 1, df2 = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pf(f_constrast_h03, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)

p_value_constrast_h03 <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                                   side == "less" ~ lower_tail,
                                   side == "greater" ~ upper_tail)

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
