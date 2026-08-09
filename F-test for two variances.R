#F-test for two variances

sample_one <- c()

sample_two <- c()

both_samples <- tibble(sample_one = sample_one, sample_two = sample_two)

point <- 

alpha <- 

s_one <- sd(both_samples$sample_one)

s_two <- sd(both_samples$sample_two)

variance_one <- s_one ^ 2

variance_two <- s_two ^ 2

n_one <- length(both_samples$sample_one)

n_two <- length(both_samples$sample_two)

diff_in_variances <- variance_one - variance_two

f_statistic <- variance_one / variance_two

df_one <- n_one - 1

df_two <- n_two - 1

# p value

one_sided_p_value <- if_else(
  f_statistic > qf(p = 1 - alpha / 2, df1 = df_one, df2 = df_two),
  pf(1 / f_statistic, df1 = df_one, df2 = df_two, lower.tail = TRUE),
  pf(f_statistic, df1 = df_one, df2 = df_two, lower.tail = TRUE))
  
two_sided_p_value <- 2 * one_sided_p_value

#CI

ratio_of_variances <- variance_one / variance_two

f_critical <- qf(p = 1 - alpha, df1 = df_one, df2 = df_two)

two_sided_f_critical <- qf(p = 1 - alpha / 2, df1 = df_one, df2 = df_two)

ci <- c(ratio_of_variances * 1 / two_sided_f_critical, ratio_of_variances * two_sided_f_critical)

# Results           

variance_one

variance_two

diff_in_variances

f_statistic

df_one

df_two

two_sided_f_critical

ci

one_sided_p_value

two_sided_p_value