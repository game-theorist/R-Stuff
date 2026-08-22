#Tribble making inverted

library(tidyverse)
library(labelled)

sample_data <- tibble(
  values = c(58.2,
             56.3,
             50.1,
             52.9,
             57.2,
             54.5,
             54.2,
             49.9,
             58.4,
             57.0,
             55.4,
             50.0,
             55.8,
             55.3,
             NA,
             51.7,
             54.9,
             NA,
             NA,
             NA),
  treatment = rep_len(c(1,
                        2,
                        3,
                        4), length(values)),

) |> 
  relocate(treatment) |> 
  arrange(treatment)

sample_data