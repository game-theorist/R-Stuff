#T-test for two independent means

explanatory <- summarized_tribble$

explanatory_level_one <- ""

explanatory_level_two <- ""

response <- sample_tribble$

point <- 

alpha <- 

side <- ""

x_one_hat <- summarized_tribble |> 
  filter(explanatory == explanatory_level_one) |> 
  pull(mean)

x_two_hat <- summarized_tribble |> 
  filter(explanatory == explanatory_level_two) |> 
  pull(mean)

diff_in_means <- x_one_hat - x_two_hat

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

#F-test for variances p-value

variance_one_sided_p_value <- if_else(
  f_statistic > qf(p = 1 - alpha / 2, df1 = df_one, df2 = df_two),
  pf(1 / f_statistic, df1 = df_one, df2 = df_two, lower.tail = TRUE),
  pf(f_statistic, df1 = df_one, df2 = df_two, lower.tail = TRUE))

variance_two_sided_p_value <- 2 * variance_one_sided_p_value

#T-statistic

equal_vars <- variance_two_sided_p_value > alpha

degrees_of_freedom <- if_else(equal_vars,
                              n_one + n_two - 2,
                              min(df_one, df_two))

pooled_variance <- ((n_one - 1) * variance_one + (n_two - 1) * variance_two) / (n_one + n_two - 2)

se <- if_else(
  equal_vars,
  sqrt(pooled_variance * (1 / n_one + 1 / n_two)),
  sqrt((variance_one / n_one) + (variance_two / n_two))
)

t_score <- (diff_in_means - point) / se
            
#T-test p-value

lower_tail <- pt(t_score, df = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pt(t_score, df = degrees_of_freedom, lower.tail = FALSE)

p_value <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                     side == "less" ~ lower_tail,
                     side == "greater" ~ upper_tail)                          

#CI

t_critical <- qt(p = 1 - alpha, df = degrees_of_freedom)
            
two_sided_t_critical <- qt(p = 1 - alpha / 2, df = degrees_of_freedom)
            
confidence_interval <- c(diff_in_means - se * two_sided_t_critical, diff_in_means + se * two_sided_t_critical)

# Results

results <- tibble(
  explanatory = c(explanatory_level_one, explanatory_level_two),
  x_hat = c(x_one_hat, x_two_hat),
  sd = c(s_one, s_two),
  var = c(variance_one, variance_two),
  n = c(n_one, n_two)
  )

results

diff_in_means

se

degrees_of_freedom

t_score

two_sided_t_critical

confidence_interval

p_value