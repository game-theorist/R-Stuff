library(tidyverse)
library(patchwork)

# Tribble making

sample_data <- tribble(~"treatment",
                       ~"Less than 6 hours",
                       ~"6 to 8 hours",
                       ~"More than 8 hours",
                       ~"total",
                       "Non-transportation workers",
                       35,
                       193,
                       64,
                       292,
                       "Transportation workers",
                       104,
                       499,
                       192,
                       795,
                       "total",
                       139,
                       692,
                       256,
                       1087
)

# Tribble cleaning

chisq_sample <- sample_data |> 
  select(!total) |>
  filter(!treatment == "total") |> 
  pivot_longer(
    cols = !treatment,
    names_to = "response",
    values_to = "count"
  ) |> 
  mutate(across(where(is.character), as.factor)
  ) |> 
  mutate(
    treatment_total = sum(count),
    .by = treatment
  ) |> 
  mutate(
    response_total = sum(count),
    .by = response
  ) |>
  mutate(
    proportion = count / sum(count),
    expected_count = treatment_total * (response_total / sum(count))
  ) |> 
  group_by(treatment, response) |>
  summarize(
    count = count,
    proportion = proportion,
    expected_count = expected_count,
    .groups = "drop"
  ) |> 
  mutate(chisq_fraction = ((count - expected_count) ^ 2) / expected_count
  ) |> 
  arrange(treatment)

levels_treatment <- chisq_sample |> 
  summarize(
    levels_treatment = nlevels(treatment)
  ) |> 
  pull()

levels_response <- chisq_sample |> 
  summarize(
    levels_response = nlevels(response)
  ) |> 
  pull()

