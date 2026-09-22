#Linear Regression Normality Testing

#Checking normality of residuals

library(qqplotr)
library(patchwork)

plot_residuals <- linear_regression_tibble |> 
  ggplot(aes(sample = residual)) +
  stat_qq_point() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_scaled_residuals <- linear_regression_tibble |> 
  ggplot(aes(sample = residual)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_residuals_versus_fitted <- linear_regression_tibble |> 
  ggplot(aes(x = y_hat, y = residual)) +
  geom_jitter(width = 0, height = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

plot_residuals_versus_treatment <- linear_regression_tibble |> 
  ggplot(aes(x = x_1, y = residual)) +
  geom_jitter(width = 0, height = 0.5) +
  geom_hline(yintercept = 0, color = "blue", linewidth = 1)

plot_residuals + plot_scaled_residuals + plot_residuals_versus_fitted_means + plot_residuals_versus_treatment
