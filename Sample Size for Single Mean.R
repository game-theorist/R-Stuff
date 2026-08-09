#Minimum N

alpha <- 

sigma <- 
  
critical_z_score <- qnorm(p = 1 - alpha / 2)

me <- critical_z_score * sigma

n <- critical_z_score ^ 2 * sigma ^ 2 / me ^ 2

n