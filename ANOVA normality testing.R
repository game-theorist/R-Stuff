#ANOVA normality testing

#Checking normality of residuals

library(qqplotr)
library(patchwork)

plot_residuals <- residuals |> 
  ggplot(aes(sample = residual)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_scaled_residuals <- residuals |> 
  ggplot(aes(sample = scaled_residual)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_run_order <- residuals |> 
  ggplot(aes(x = test_sequence, y = residual)) +
  geom_point()

plot_versus_fitted <- residuals |> 
  ggplot(aes(x = residual, y = observed_value)) +
  geom_jitter() + 
  geom_smooth(method = "lm", se = FALSE)

plot_residuals + plot_scaled_residuals

plot_run_order + plot_versus_fitted

#Bartlet's test

variance_pop <- summarized_tribble |> 
  summarize(variance_pop = sum((group_n - 1) * treatment_variance) / (n - levels)) |> 
  pull()

q <- summarized_tribble |> 
  summarize(q = (n - levels) * log10(variance_pop) - sum((group_n - 1) * log10(treatment_variance))) |> 
  pull()

c <- summarized_tribble |> 
  summarize(c = 1 + (1 / (3 * (levels - 1)) * (sum( 1 / (group_n - 1)) - (1 / (n - levels))))) |> 
  pull()

bartlet_chisq <- 2.3026 * (q / c)

degrees_of_freedom <- levels - 1

p_value <- pchisq(bartlet_chisq, df = degrees_of_freedom, lower.tail = FALSE)

#Levene deviation test

lower_tail <- pf(f_statistic, df1 = df_treatment, df2 = df_error, lower.tail = TRUE)

upper_tail <- pf(f_statistic, df1 = df_treatment, df2 = df_error, lower.tail = FALSE)

p_value <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                     side == "less" ~ lower_tail,
                     side == "greater" ~ upper_tail)