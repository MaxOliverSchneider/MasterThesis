indep_norm_dist_matrix <- function(nrow, ncol, mean = 0, sd = 1, as_dataframe = TRUE){
  '
  Generates a matrix of independent normally distributed variables
  '
  vars <- matrix(nrow = nrow, ncol = ncol)
  vars[,1:ncol] <- rnorm(n, mean = mean, sd = sd)
  if (as_dataframe){vars = data.frame(vars)}
  return(vars)
}

noise = indep_norm_dist_matrix(1000, 4, 10, 2)
dep = indep_norm_dist_matrix(1000, 1, 100, 2)
exp_1 = dep + rnorm(length(dep), -2, 10)

#Generate multiple explanatory variables, with varying degree of error size in one function

#Glue together variables
dataset = cbind(noise, dep, exp_1)
dataset = data.frame(dataset)
#Plot multiple variables

#Run regression algorithms
reg <- lm(vars~vars.1, data = dataset)

