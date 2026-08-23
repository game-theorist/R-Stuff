#T-test for a single mean

mu_0 <- 225

side <- "greater"

alpha <- 0.05

t_tribble <- summarized_tribble |> 
  mutate(diff_in_means = treatment_mean - mu_0,
         se = treatment_sd / sqrt(treatment_n),
         t_statistic = diff_in_means / se,
         t_df = treatment_n - 1,
         p_value =  case_when(side == "two-sided" ~ 2 * pmin(pt(t_statistic, t_df, lower.tail = FALSE), pt(t_statistic, t_df, lower.tail = TRUE)),
                              side == "less" ~ pt(t_statistic, t_df, lower.tail = TRUE),
                              side == "greater" ~ pt(t_statistic, t_df, lower.tail = FALSE)),
         conf_lower = treatment_mean - (abs(qt(alpha / 2, t_df ) * se)),
         conf_upper = treatment_mean + (abs(qt(alpha / 2, t_df ) * se))
  ) |> 
  select(!starts_with("treatment"))

t_tribble