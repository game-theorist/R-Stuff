#T-test for two DEPENDENT means

explanatory <- summarized_tribble$

explanatory_level_one <- ""

explanatory_level_two <- ""

response <- sample_tribble$

point <- 
  
alpha <- 

side <- ""

n <- nrow(sample_tribble) / 2
  
x_one_hat <- summarized_tribble |> 
  filter(explanatory == explanatory_level_one) |> 
  pull(mean)
  
x_two_hat <- summarized_tribble |> 
  filter(explanatory == explanatory_level_two) |> 
  pull(mean)

mean_diffs <- diffs |> 
  summarize(mean(diffs)) |> 
  pull()

# Sample standard deviation of the differences

sd_diff <- sqrt(sum((diffs$diffs - mean_diffs) ^ 2) / (n - 1))

# SE of diffs

se_diff <- sd_diff / sqrt(n)

#Testing for variances

variance_one <- summarized_tribble |> 
  filter(explanatory == explanatory_level_one) |> 
  pull(variance) 

variance_two <- summarized_tribble |> 
  filter(explanatory == explanatory_level_two) |> 
  pull(variance)

s_one <- sqrt(variance_one)
  
s_two <- sqrt(variance_two)

n_one <- summarized_tribble |> 
  filter(explanatory == explanatory_level_one) |> 
  pull(n) 

n_two <- summarized_tribble |> 
  filter(explanatory == explanatory_level_two) |> 
  pull(n)

diff_in_variances <- variance_one - variance_two

f_statistic <- variance_one / variance_two

df_one <- n_one - 1

df_two <- n_two - 1

#F-test for the variance's p-value

variance_one_sided_p_value <- if_else(
  f_statistic > qf(p = 1 - alpha / 2, df1 = df_one, df2 = df_two),
  pf(1 / f_statistic, df1 = df_one, df2 = df_two, lower.tail = TRUE),
  pf(f_statistic, df1 = df_one, df2 = df_two, lower.tail = TRUE))

variance_two_sided_p_value <- 2 * variance_one_sided_p_value

#T-statistic

t_score <- (mean_diffs - point) / se_diff

degrees_of_freedom <- if_else(
  variance_two_sided_p_value > alpha,
  n_one + n_two - 2,
  min(c(n_one - 1, n_two - 1)))

#T-test p-value

lower_tail <- pt(t_score, df = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pt(t_score, df = degrees_of_freedom, lower.tail = FALSE)

p_value <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                     side == "less" ~ lower_tail,
                     side == "greater" ~ upper_tail)                          

#Confidence Interval

t_critical <- qt(p = 1 - alpha, df = degrees_of_freedom)

two_sided_t_critical <- qt(p = 1 - alpha / 2, df = degrees_of_freedom)

confidence_interval <- c(mean_diffs - sd_diff * two_sided_t_critical, mean_diffs + sd_diff * two_sided_t_critical)

# Results

results <- tibble(
  explanatory = c(explanatory_level_one, explanatory_level_two),
  x_hat = c(x_one_hat, x_two_hat),
  sd = c(s_one, s_two)
)

results

mean_diffs

diff_in_variances

se

degrees_of_freedom

t_score

t_critical

confidence_interval

p_value