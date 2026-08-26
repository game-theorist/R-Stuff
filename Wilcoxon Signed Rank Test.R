#Wilcoxon's Signed Rank Test

library(tidyverse)
library(janitor)

side <- "two-sided"

alpha <- 0.05

null_difference <- 0

wilcoxon_signed_tribble <- raw_tribble |>
  mutate(raw_difference = observed_value - lead(observed_value, n = n() / 2),
         sign = case_when(raw_difference == null_difference ~ NA,
                          raw_difference > null_difference ~ "positive",
                          raw_difference < null_difference ~ "negative"),
         abs_difference = abs(raw_difference)
  ) |> 
  drop_na(sign) |>
  arrange(abs_difference) |>
  mutate(rank = row_number()
  ) |>
  mutate(rank = mean(rank), .by = (abs_difference)
  ) 

wilcoxon_signed_summarized_tribble <- wilcoxon_signed_tribble |>
  group_by(sign)|>
  summarize(
    sum = sum(rank),
    n = n()
  ) |> 
  pivot_wider(
    names_from = sign,
    names_glue = "{sign}_sum",
    values_from = sum
  ) |> 
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))
  ) |> 
  mutate(smaller_sum = pmin(negative_sum, positive_sum),
         p_value = case_when(side == "two-sided" ~ 2 * pmin(psignrank(q = smaller_sum, n = n, lower.tail = TRUE),
                                                            psignrank(q = smaller_sum, n = n, lower.tail = FALSE)),
                             side == "less" ~ psignrank(q = smaller_sum, n = n, lower.tail = TRUE),
                             side == "greater" ~ psignrank(q = smaller_sum, n = n, lower.tail = FALSE))
  )

wilcox.test(raw_tribble$observed_value[11:20], raw_tribble$observed_value[1:10], paired = TRUE, exact = FALSE)

wilcoxon_signed_tribble

wilcoxon_signed_summarized_tribble
