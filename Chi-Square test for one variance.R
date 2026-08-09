#Chi-Square test for one variance

library(DescTools)

VarTest(sample_tribble$etch, sigma.squared = sigma_squared, alternative = "two.sided")

sigma_squared <- 1

s_squared <- sd(sample_tribble$etch) ^ 2

diff_in_variances <- s_squared - sigma_squared

sum_of_squares <- sum((sample_tribble$etch - summarized_tribble$mean) ^ 2)

chisq <- sum_of_squares / sigma_squared

degrees_of_freedom <- summarized_tribble$n - 1


#Chisq test for p-value

chisq_critical <- qchisq(p = alpha / 2, df = degrees_of_freedom)

chisq_one_sided_p_value <- if_else(
  chisq > chisq_critical,
  pchisq(chisq, df = degrees_of_freedom, lower.tail = FALSE),
  pchisq(chisq, df = degrees_of_freedom, lower.tail = TRUE))

chisq_two_sided_p_value <- 2 * chisq_one_sided_p_value

#CI#

two_sided_chisq_critical <- qchisq(p = 1 - alpha / 2, df = degrees_of_freedom)

confidence_interval <- c((sum_of_squares / two_sided_chisq_critical), (sum_of_squares / chisq_critical))

#Results

chisq

chisq_critical

chisq_one_sided_p_value

chisq_two_sided_p_value

confidence_interval
