#Tribble making inverted

library(tidyverse)
library(labelled)

sample_data <- tibble(
  values = c(
             39.7,
             52.9,
             59.1,
             56.7,
             56.1,
             61.9,
             57.7,
             71.4,
             60.6,
             67.7,
             37.8,
             50.0,
             58.2,
             60.7,
             33.6,
             51.3,
             56.0,
             59.5,
             65.3,
             59.8
             ),
  treatment = rep_len(c("admission", "sixhrs"), length(values)),

) |> 
  relocate(treatment) |> 
  arrange(treatment)

sample_data