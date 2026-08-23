#T-test for two independent means

#Testing for equality of variances

equal_variance_alpha <- 0.05

equal_variance_tribble <- summarized_tribble |> 
  mutate(f_statistic = treatment_variance / lead(treatment_variance),
         df_one = treatment_n[1] - 1,
         df_two = treatment_n[2] - 1
  ) |> 
  drop_na() |> 
  mutate(p_value = 2 * pmin(pf(f_statistic, df_one, df_two, lower.tail = TRUE), pf(f_statistic, df_one, df_two, lower.tail = FALSE)),
         equal = if_else(f_statistic > qf(equal_variance_alpha / 2, df_one, df_two, lower.tail = FALSE), "no", "yes")
  ) |> 
  select(!starts_with("treatment"))

#Student's T Test

student_tribble <- summarized_tribble |> 
  mutate(diff_in_means = treatment_mean - lead(treatment_mean),
         pooled_variance = sum(treatment_variance * (treatment_n - 1)) / (sum(treatment_n) - 2),
         pooled_sd = sqrt(pooled_variance),
         student_t_statistic = diff_in_means / (pooled_sd * sqrt(sum(1 / treatment_n))),
         student_df = sum(treatment_n) - 2,
         two_sided_p_value =  2 * pmin(pt(student_t_statistic, student_df, lower.tail = FALSE), pt(student_t_statistic, student_df, lower.tail = TRUE))
  ) |> 
  drop_na() |> 
  select(!starts_with("treatment"))

#Welch's T test

welch_tribble <- summarized_tribble |> 
  mutate(diff_in_means = treatment_mean - lead(treatment_mean),
         separate_variance = sqrt(sum(treatment_variance / treatment_n)),
         welch_t_statistic = diff_in_means / separate_variance,
         welch_df = (sum(treatment_variance / treatment_n) ^ 2) / sum(((treatment_variance / treatment_n) ^ 2) / (treatment_n - 1)),
         two_sided_p_value =  2 * pmin(pt(welch_t_statistic, welch_df, lower.tail = FALSE), pt(welch_t_statistic, welch_df, lower.tail = TRUE))
  ) |> 
  drop_na() |> 
  select(!starts_with("treatment"))
