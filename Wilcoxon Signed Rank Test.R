#Wilcoxon's Signed Rank Test

side <- "greater"

alpha <- 0.05

null_difference <- 0

wilcoxon_signed_tribble <- raw_tribble |>
  mutate(raw_difference = - observed_value + lead(observed_value, n = n / 2),
         sign = case_when(raw_difference == null_difference ~ NA,
                          raw_difference > null_difference ~ "positive",
                          raw_difference < null_difference ~ "negative"),
         abs_difference = abs(raw_difference)
  ) |> 
  drop_na() |>
  arrange(abs_difference) |>
  mutate(rank = row_number()
  ) |> 
  group_by(sign) |> 
  summarize(
    sum = sum(rank)
  ) |> 
  pivot_wider(
    names_from = sign,
    names_glue = "{sign}_sum",
    values_from = sum
  )

pbinom(q = wilcoxon_signed_tribble$positive_sum,
       size = n,
       p = 0.5
       )

wilcoxon_signed_tribble