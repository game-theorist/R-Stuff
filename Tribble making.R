#Tribble making

library(tidyverse)
library(labelled)

sample_data <- tribble(~"treatment",
                       ~"values",
                       ~"values",
                       "Compact",
                       1,
                       3,
                       4,
                       7,
                       5,
                       6,
                       3,
                       2,
                       1,
                       7,
                       5,
                       "Midsize",
                       4,
                       1,
                       3,
                       5,
                       7,
                       1,
                       2,
                       4,
                       2,
                       7,
                       4
)

sample_data