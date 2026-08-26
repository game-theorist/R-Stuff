#Tribble making

library(tidyverse)
library(labelled)

sample_data <- tribble(~"treatment",
                       ~"values",
                       ~"values",
                       ~"values",
                       ~"values",
                       ~"values",
                       ~"values",
                       ~"values",
                       "Polluted",
                       21.3 ,
                       18.7,
                       23.0 ,
                       17.1,
                       16.8,
                       20.9,
                       19.7,
                       "Unpolluted",
                       14.2,
                       18.3,
                       17.2,
                       18.4,
                       20.0,
                       NA,
                       NA
)

sample_data