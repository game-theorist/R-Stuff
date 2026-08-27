library(tidyverse)

# Tribble making

sample_data <- tribble(~"treatment",
                       ~"success",
                       ~"failure",
                       "exposure",
                       12,
                       62,
                       "non_exposure",
                       13,
                       103
)

# Tribble cleaning

raw_tribble <- sample_data |> 
  #select(!total) |>
  #filter(!treatment == "total") |> 
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
  )

summarized_tribble <- raw_tribble |> 
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

levels_treatment <- raw_tribble  |> 
  summarize(
    levels_treatment = nlevels(treatment)
  ) |> 
  pull()

levels_response <- raw_tribble  |> 
  summarize(
    levels_response = nlevels(response)
  ) |> 
  pull()