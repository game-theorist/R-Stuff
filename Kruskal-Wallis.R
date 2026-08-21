library(tidyverse)

# Kruskal Wallis

kruskal_tribble <- raw_tribble |> 
  arrange(observed_value) |> 
  mutate(raw_kruskal_rank = row_number()
         ) |> 
  group_by(observed_value) |>
  mutate(kruskal_rank = mean(raw_kruskal_rank)
  ) |> 
  ungroup()
  
kruskal_summarized_tribble <- kruskal_tribble |> 
  group_by(treatment) |> 
  summarize(kruskal_sum = sum(kruskal_rank),
            kruskal_mean = mean(kruskal_rank),
            treatment_n = n())
  
kruskal_df <- levels - 1

kruskal_rank_variance <- (((sum(kruskal_tribble$kruskal_rank ^ 2)) - (((n * ((n + 1) ^ 2)) / 4))) / (n -1))

kruskal_h <- (sum((kruskal_summarized_tribble$kruskal_sum ^ 2) / kruskal_summarized_tribble$treatment_n) - (((n * ((n + 1) ^ 2)) / 4))) / kruskal_rank_variance

kruskal_p_value <- pchisq(kruskal_h, kruskal_df, lower.tail = FALSE)

kruskal_p_value