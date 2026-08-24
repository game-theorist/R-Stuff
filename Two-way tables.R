chisq_sample <- sample_data |> 
  select(!Total) |> 
  mutate(across(everything(), as.factor)) |>
   add_count() |> 
   mutate(
     question_total = n(),
     .by = treatment
   ) |> 
   mutate(
     response_total = n(),
     .by = response
   ) |>
   mutate(
     expected_count = question_total * (response_total / mean(n))
   ) |> 
   group_by(treatment, response) |>
   summarize(
     count = n(),
     expected_count = mean(expected_count),
     .groups = "drop"
   ) |> 
   mutate(chisq_fraction = ((count - expected_count) ^ 2) / expected_count
   )

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

  
ggplot(aes(x = treatment, fill = response)) +
geom_bar()