library(qqplotr)
library(patchwork)

t_test_qq_plot <- raw_tribble |>
  ggplot(aes(sample = observed_value)) +
  stat_qq_point() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw()

t_test_pp_plot <- raw_tribble |> 
  ggplot(aes(sample = observed_value)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

t_test_qq_plot + t_test_pp_plot