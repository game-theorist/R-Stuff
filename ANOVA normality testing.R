#ANOVA normality testing

#Checking normality of residuals

library(qqplotr)
library(patchwork)

plot_residuals <- residuals |> 
  ggplot(aes(sample = residual)) +
  stat_qq_point() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_scaled_residuals <- residuals |> 
  ggplot(aes(sample = residual)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_residuals_versus_fitted_means <- residuals |> 
  ggplot(aes(x = treatment_mean, y = residual)) +
  geom_jitter(width = 0, height = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

plot_residuals_versus_treatment <- residuals |> 
  ggplot(aes(x = treatment, y = residual)) +
  geom_jitter(width = 0, height = 0.5) +
  geom_hline(yintercept = 0, color = "blue", linewidth = 1)

plot_residuals + plot_scaled_residuals + plot_residuals_versus_fitted_means + plot_residuals_versus_treatment

#plot_run_order <- residuals |> 
 # ggplot(aes(x = test_sequence, y = residual)) +
  #geom_point()

#Bartlet's test

variance_pop <- summarized_tribble |> 
  summarize(variance_pop = sum((treatment_n - 1) * treatment_variance) / (n - levels)) |> 
  pull()

q <- summarized_tribble |> 
  summarize(q = (n - levels) * log10(variance_pop) - sum((treatment_n - 1) * log10(treatment_variance))) |> 
  pull()

c <- summarized_tribble |> 
  summarize(c = 1 + (1 / (3 * (levels - 1)) * (sum( 1 / (treatment_n - 1)) - (1 / (n - levels))))) |> 
  pull()

bartlet_chisq <- 2.3026 * (q / c)

bartlet_df <- levels - 1

bartlet_p_value <- pchisq(bartlet_chisq, df = bartlet_df, lower.tail = FALSE)

#Levene deviation test

levene_p_value <- pf(f_statistic, df1 = df_treatment, df2 = df_error, lower.tail = FALSE)