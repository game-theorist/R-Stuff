#Tibble making

sample_data <- tribble(~"Estimation Method",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       ~"Observations",
                       1,
                       0.34,
                       0.12,
                       1.23,
                       0.70,
                       1.75,
                       0.12,
                       2,
                       0.91,
                       2.94,
                       2.14,
                       2.36,
                       2.86,
                       4.55,
                       3,
                       6.31,
                       8.37,
                       9.75,
                       6.09,
                       9.82,
                       7.24,
                       4,
                       17.15,
                       11.82,
                       10.95,
                       17.20,
                       14.35,
                       16.82
                       )

sample_data

sample_tribble <- sample_data |> 
  pivot_longer(cols = "Observations",
               names_to = NULL,
               values_to = "measurement"
               )|> 
  mutate(Method = as.factor(`Estimation Method`), .before = 1) |> 
  mutate(`Estimation Method` = NULL)

sample_tribble

# Unpaired Samples

sample_tribble <- sample_data |> 
  pivot_longer(cols = everything(),
               names_to = "solution",
               names_transform = list(solution = ~ as.factor(str_remove_all(.x, "Solution "))),
               values_to = "etch_rate"
               )

summarized_tribble <- sample_tribble |> 
  group_by(solution) |> 
  summarize(mean = mean(etch_rate),
            sd = sd(etch_rate),
            variance = var(etch_rate),
            n = n())

sample_tribble

summarized_tribble


# Paired Samples

sample_tribble <- sample_data |> 
  mutate(wafer = as.factor(c(1:8))) |> 
  pivot_longer(cols = c("Solution 1", "Solution 2"), 
               names_to = "solution",
               names_transform = list(solution = ~ as.factor(str_remove_all(.x, "Solution "))),
               values_to = "etch_rate"
               )


summarized_tribble <- sample_tribble |> 
  group_by(solution) |> 
  summarize(mean = mean(etch_rate),
            sd = sd(etch_rate),
            variance = var(etch_rate),
            n = n())

diffs <- sample_data |> 
  mutate(diffs = `Solution 1` - `Solution 2`) 

sample_tribble

summarized_tribble