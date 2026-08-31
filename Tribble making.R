#Tribble making

# Words

##(\b[a-z]+\b)
##"$1",\n

# Numbers

##([0-9]+\.?[0-9]*)[\r\n]+[ \t]*
##$1,\n

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
                       ~"values",
                       ~"values",
                       ~"values",
                       1,
4.079,
4.859,
3.540,
5.047,
3.298,
4.679,
2.870,
4.648,
3.847,
NA,
2,
4.368,
5.668,
3.752,
5.848,
3.802,
4.844,
3.578,
5.393,
4.374,
NA,
3,
4.169,
5.709,
4.416,
5.666,
4.123,
5.059,
4.403,
4.496,
4.688,
NA,
4,
4.928,
5.608,
4.940,
5.291,
4.674,
5.038,
4.905,
5.208,
4.806,
NA
)

sample_data