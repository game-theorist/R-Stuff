# ANOVA sample size

# Cohen's F

cohen_f <- sqrt((sum((summarized_tribble$treatment_mean - mean(summarized_tribble$treatment_mean)) ^ 2) / levels) / sum(summarized_tribble$treatment_variance)) 

lambda <- n * levels * cohen_f

f_power <- mean_squares_treatment / mean_squares_error

pf(f_power, df1 = levels - 1, df2 = n - levels, ncp = lambda)



# Cohen's F

means <- c(575, 600, 650, 675)

cohen_f <- sqrt((sum((means - mean(means)) ^ 2) / levels) / ((25 ^ 2) * 4))  

lambda <- n * levels * (cohen_f ^ 2)

f_power <- mean_squares_treatment / mean_squares_error

pf(1 - f_power, df1 = levels - 1, df2 = n - levels, ncp = lambda, lower.tail = FALSE)