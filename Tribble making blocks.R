#Tribble making inverted

library(tidyverse)
library(labelled)

# Numbers

##([0-9]+\.?[0-9]*)[\r\n]+[ \t]*
##$1,\n

sample_data <- tibble(
  values = c(

90.3,
89.2,
98.2,
93.9,
87.4,
97.9,
92.5,
89.5,
90.6,
94.7,
87.0,
95.8,
85.5,
90.8,
89.6,
86.2,
88.0,
93.4,
82.5,
89.5,
85.6,
87.4,
78.9,
90.7
),
  treatment = rep(c("8500",
                        "8700",
                        "8900",
                        "9100"), length.out = length(values), each = 6),
  block = rep_len(c(1:6), length(values))

) |> 
  relocate(treatment, block) |> 
  arrange(treatment)

sample_data