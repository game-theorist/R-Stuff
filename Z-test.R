#Z-test and confidence interval

sample_data <- c()

alpha <- 

mu <- 

sigma <- 
  
x_hat <- mean(sample_data)

n <- length(sample_data)

se <- sigma / sqrt(n)

z_score <- (x_hat - mu) / se


# p value

one_sided_p_value <- pnorm(z_score, lower.tail = FALSE)

two_sided_p_value <- 2 * one_sided_p_value

#CI

z_critical <- qnorm(p = 1 - alpha / 2)

ci <- c(x_hat - se * z_critical, x_hat + se * z_critical)

x_hat

sigma

n

se

z_score

z_critical

ci

one_sided_p_value

two_sided_p_value