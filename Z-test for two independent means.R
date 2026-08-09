#Z-test for two independent means

explanatory <- summarized_tribble$
  
  explanatory_level_one <- ""

explanatory_level_two <- ""

response <- sample_tribble$

sigma_one <- 2.30

sigma_two <- 4.03

x_one_hat <- 2.69

x_two_hat <- 6.35

diff_in_means <- x_one_hat - x_two_hat

s_one <- 2.30

s_two <- 4.03

n_one <- 43

n_two <- 45

point <- -x_one_hat

alpha <- 0.05

side <- "greater"

degrees_of_freedom <- min(c(n_one - 1, n_two - 1))

se <- sqrt((sigma_one ^ 2 / n_one) + (sigma_two ^ 2 / n_two))

z_score <- (diff_in_means - point) / se

# p value

lower_tail <- pt(t_score, df = degrees_of_freedom, lower.tail = TRUE)

upper_tail <- pt(t_score, df = degrees_of_freedom, lower.tail = FALSE)

p_value <- case_when(side == "two_sided" ~ 2 * pmin(lower_tail, upper_tail),
                     side == "less" ~ lower_tail,
                     side == "greater" ~ upper_tail)  

#CI

z_critical <- qnorm(p = 1 - alpha)

two_sided_z_critical <- qnorm(p = 1 - alpha / 2)

ci <- c(diff_in_means - se * two_sided_z_critical, diff_in_means + se * two_sided_z_critical)

# Results           

x_one_hat

x_two_hat

diff_in_means

s_one

s_two

se

degrees_of_freedom

z_score

z_critical

ci

p_value