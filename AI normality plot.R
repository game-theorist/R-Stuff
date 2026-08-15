library(tidyverse)
library(qqplotr)
library(patchwork)

# 1. Parse your exact unpaired data
sample_tribble <- tibble(
  Duration = rep(c("Shorter", "Longer"), each = 20),
  score = c(
    1, 3, 2, 6, 1, 5, 3, 3, 5, 2, 1, 1, 5, 6, 2, 8, 3, 2, 5, 3, # Shorter points
    7, 6, 8, 9, 5, 5, 9, 7, 5, 4, 8, 6, 6, 8, 4, 5, 6, 8, 7, 7  # Longer points
  )
)

# 2. Standardize the scores within each group (creates Z-scores: mean = 0, sd = 1)
sample_scaled <- sample_tribble |> 
  group_by(Duration) |> 
  mutate(z_score = scale(score)) |> 
  ungroup()

# 3. Create the Shorter plot using Z-scores (everything defaults to mean=0, sd=1)
plot_shorter <- ggplot(filter(sample_scaled, Duration == "Shorter"), aes(sample = z_score)) +
  stat_qq_band(distribution = "norm", alpha = 0.2, fill = "purple") +
  stat_qq_line(color = "darkgray", linewidth = 0.8) +
  stat_qq(size = 2, color = "purple") + 
  labs(title = "Shorter Duration (Standardized)", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles (Z-score)") +
  theme_bw()

# 4. Create the Longer plot using Z-scores
plot_longer <- ggplot(filter(sample_scaled, Duration == "Longer"), aes(sample = z_score)) +
  stat_qq_band(distribution = "norm", alpha = 0.2, fill = "green") +
  stat_qq_line(color = "darkgray", linewidth = 0.8) +
  stat_qq(size = 2, color = "green") + 
  labs(title = "Longer Duration (Standardized)", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles (Z-score)") +
  theme_bw()

# 5. Display side-by-side
plot_shorter + plot_longer
