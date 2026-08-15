#T-test and confidence interval for single mean

sample_data <- c()

alpha <- 
  
mu <- 
  
s <- sd(sample_data)
  
x_hat <- mean(sample_data)

n <- length(sample_data)

se <- s / sqrt(n)

t_score <- (x_hat - mu) / se


# p value

one_sided_p_value <- pt(t_score, df = n - 1, lower.tail = FALSE)

two_sided_p_value <- 2 * p_value

#CI

t_critical <- qt(p = 1 - alpha, df = n - 1)

two_sided_t_critical <- qt(p = 1 - alpha / 2, df = n - 1)

ci <- c(x_hat - se * two_sided_t_critical, x_hat + se * two_sided_t_critical)

x_hat

se

n

t_score

t_critical

ci

one_sided_p_value

two_sided_p_value