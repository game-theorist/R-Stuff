#Tribble making inverted

library(tidyverse)
library(labelled)

sample_data <- tibble(
  values = c(
    120.07,
    114.48,
    50.54,
    87.43,
    100.19,
    26.74,
    194.72,
    104.61,
    66.58,
    101.88,
    121.73,
    25.68,
    74.64,
    85.03,
    63.81,
    52.14,
    110.31,
    5.64,
    42.19,
    151.75,
    7.33,
    42.89,
    75.63,
    37.69,
    112.29,
    162.72,
    1.79,
    131.27,
    32.03,
    53.94,
    16.94,
    137.94,
    21.81,
    136.71,
    11.21,
    43.74,
    25.30,
    3.36,
    49.77,
    NA,
    17.20,
    24.34,
    NA,
    4.48,
    NA),
  treatment = rep_len(c("Vehicle",
                        "Compound X",
                        "Compound Y"), length(values)),

) |> 
  relocate(treatment) |> 
  arrange(treatment)

sample_data