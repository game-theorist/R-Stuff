#Tribble making inverted

library(tidyverse)
library(labelled)

sample_data <- tibble(
  values = c(1,
             3,
             7,
             6,
             2,
             6,
             8,
             9,
             1,
             5,
             5,
             5,
             3,
             3,
             9,
             7,
             5,
             2,
             5,
             4,
             1,
             1,
             8,
             6,
             5,
             6,
             6,
             8,
             2,
             8,
             4,
             5,
             3,
             2,
             6,
             8,
             5,
             3,
             7,
             7),
  treatment = rep_len(c(10,
                        10,
                        20,
                        20), length(values)),

) |> 
  relocate(treatment) |> 
  arrange(treatment)

sample_data