#Mann-Whitney Test / Wilcoxon's Rank Sum

library(tidyverse)
library(janitor)

side <- "greater"

alpha <- 0.05

mann_whitney_tribble <- raw_tribble |> 
  drop_na(observed_value) |> 
  arrange(observed_value) |>
  mutate(rank = row_number()
  ) |>
  mutate(rank = mean(rank), .by = (observed_value)
  ) 

mann_whitney_summarized_tribble <- mann_whitney_tribble |>
  summarize(
    sum = sum(rank),
    n = n(),
    .by = treatment
  ) |>
  arrange(treatment) |> 
  mutate(u_statistic = (n[1] * n[2]) + n * ((n + 1) / 2) - sum,
         p_value = case_when(side == "two-sided" ~ 2 * pmin(pwilcox(q = u_statistic,
                                                                    m = n[1],
                                                                    n = n[2],
                                                                    lower.tail = TRUE),
                                                            (pwilcox(q = u_statistic,
                                                                    m = n[1],
                                                                    n = n[2],
                                                                    lower.tail = FALSE))),
                             side == "less" ~ pwilcox(q = u_statistic,
                                                       m = n[1],
                                                       n = n[2],
                                                       lower.tail = TRUE),
                             side == "greater" ~ pwilcox(q = u_statistic,
                                                         m = n[1],
                                                         n = n[2],
                                                         lower.tail = FALSE))
         )

mann_whitney_summarized_tribble

wilcox.test(observed_value ~ treatment, data = mann_whitney_tribble, alternative = side)
