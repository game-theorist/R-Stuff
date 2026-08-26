#Tibble making

library(tidyverse)
library(labelled)

sample_data <- tibble(
  "adhesive 1" = c(229, 286, 245, 299, 250),
  "adhesive 2" = c(213, 179, 163, 247, 225)
) |> 
  pivot_longer(
    cols = everything(),
    names_to = "treatment",
    values_to = "values"
  ) |> 
  arrange(treatment)


sample_data