#Tribble making inverted

library(tidyverse)
library(labelled)

sample_data <- tibble(
  values = c(
             21.3,
             18.7 ,
             23.0 ,
             17.1 ,
             16.8 ,
             20.9 ,
             19.7 ,
             14.2,
             18.3 ,
             17.2,
             18.4 ,
             NA,
             20.0 ,
             NA
             ),
  treatment = rep_len(c("Polluted", "Unpolluted"), length(values)),

) |> 
  relocate(treatment) |> 
  arrange(treatment)

sample_data