#Linear regression

library(tidyverse)
library(labelled)

#Making matrices

x_matrix <- sample_data |> 
  mutate(
    ones = 1,
    across(starts_with("x_"), ~ .x),
    .keep = "used"
  ) |> 
  relocate(ones) |> 
  as.matrix()

y_vector <- sample_data |> 
  select(y_obs) |> 
  as.matrix()

#Coefficients

coefficients <- as.numeric((solve((t(x_matrix) %*% x_matrix))) %*% (t(x_matrix) %*% y_vector)) 

coefficients <- coefficients |> 
  set_names(paste0("beta_", seq_along(coefficients) - 1)
            ) |> 
  as_tibble_row()

#Tibble

linear_regression_tibble <- sample_data |> 
  bind_cols(coefficients) |> 
  mutate(
    y_hat = beta_0 + rowSums(across(starts_with("x_"), ~ .x * get(paste0("beta_", str_extract(cur_column(), "\\d+"))))),
    residual = y_obs - y_hat
  )