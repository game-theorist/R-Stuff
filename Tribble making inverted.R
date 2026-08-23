#Tribble making inverted

library(tidyverse)
library(labelled)

sample_data <- tibble(
  values = c(
             7,
             6,
             
             3,
             3,
             
             3,
             5,
             
             4,
             3,
             
             8,
             8,
             
             3,
             2,
             
             2,
             4,
             
             9,
             9,
             
             5,
             4,
             
             4,
             5),
  treatment = rep_len(c("Tip 1", "Tip 2"), length(values)),

) |> 
  relocate(treatment) |> 
  arrange(treatment)

sample_data