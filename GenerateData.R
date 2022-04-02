#Load packages
packages <- c("MASS", "scales", "clusterGeneration")
lapply(packages, require, character.only = TRUE)

###
# Create simple multivariate DS
## No correlations between variables, simple PS, simple outcome function
###

genDSmultivariate_1 <- function(n_obs = 1000,
                                treatment_effect = 10,
                                alpha_PS = 2,
                                beta_PS = 1,
                                gamma_PS = 0.1,
                                alpha_outcome = 2,
                                beta_outcome = 1,
                                gamma_outcome = 0.5,
                                n_A = 10,
                                n_B = 30,
                                n_C = 50,
                                v_range = c(-2,2),
                                e_range = c(-2,2),
                                return_Matrix = FALSE
) {
  #Create data
  A = mvrnorm(n = n_obs, mu = rep(0, n_A), Sigma = diag(n_A))
  B = mvrnorm(n = n_obs, mu = rep(0, n_B), Sigma = diag(n_B))
  C = mvrnorm(n = n_obs, mu = rep(0, n_C), Sigma = diag(n_C))
  v = runif(n_obs, min = v_range[1], max = v_range[2])
  e = runif(n_obs, min = e_range[1], max = e_range[2])
  dataset = data.frame(A, B, C)
  colnames(dataset) = c(paste0("A", 1:n_A), paste0("B", 1:n_B), paste0("C", 1:n_C))
  
  #Calculate PS and treatment indicator
  PS_unscaled = rowSums(alpha_PS * A) + rowSums(beta_PS * B) + rowSums(gamma_PS * C) + v
  PS_scaled = rescale(PS_unscaled, to = c(0,1))
  dataset["PS_scaled"] <- PS_scaled
  dataset["T"] <- rbinom(n_obs, 1, PS_scaled)
  
  #Calculate outcome
  dataset["Y"] = treatment_effect * dataset["T"] + rowSums(alpha_outcome * A) + 
    rowSums(beta_outcome * B) + rowSums(gamma_outcome * C) + e
  dataset["T"] = as.factor(dataset[["T"]])
  if (return_Matrix) {return(as.matrix(dataset))} else {return(dataset)}
}

###
# Generate data with co-dependencies
###

genDSmultivariate_2 <- function(n_obs = 1000,
                                treatment_effect = 10,
                                alpha_PS = 5,
                                beta_PS = 1,
                                gamma_PS = 0.1,
                                alpha_outcome = 2,
                                beta_outcome = 1,
                                gamma_outcome = 0.5,
                                n_A = 4,
                                n_B = 3,
                                n_C = 2,
                                v_range = c(-2,2),
                                e_range = c(-2,2),
                                return_Matrix = FALSE
) {
  #Create data
  #Alpha receives custom covariance matrix
  sigma_A <- matrix(c(1, 0.3, 2, 4, 
                      0, 1, 1, 4, 
                      0, 0, 1, 4,
                      0, 0, 0, 1)
                    ,4,4,
                    byrow = TRUE)
  A = mvrnorm(n = n_obs, mu = rep(0, n_A), Sigma = sigma_A)
  
  B = mvrnorm(n = n_obs, mu = rep(0, n_B), Sigma = diag(n_B))
  C = mvrnorm(n = n_obs, mu = rep(0, n_C), Sigma = diag(n_C))
  v = runif(n_obs, min = v_range[1], max = v_range[2])
  e = runif(n_obs, min = e_range[1], max = e_range[2])
  dataset = data.frame(A, B, C)
  colnames(dataset) = c(paste0("A", 1:n_A), paste0("B", 1:n_B), paste0("C", 1:n_C))
  
  #Calculate PS and treatment indicator
  PS_unscaled = rowSums(alpha_PS * A) + rowSums(beta_PS * B) + rowSums(gamma_PS * C) + v
  PS_scaled = rescale(PS_unscaled, to = c(0,1))
  dataset["PS_scaled"] <- PS_scaled
  dataset["T"] <- rbinom(n_obs, 1, PS_scaled)
  
  #Calculate outcome
  dataset["Y"] = treatment_effect * dataset["T"] + rowSums(alpha_outcome * A) + 
    rowSums(beta_outcome * B) + rowSums(gamma_outcome * C) + e
  dataset["T"] = as.factor(dataset[["T"]])
  if (return_Matrix) {return(as.matrix(dataset))} else {return(dataset)}
}

###
# Generate most simple multivariate set-up
###
genDS_MV_simple <- function(n_obs = 500, 
                            n_X = 5,
                            X_dist = "normal_no_cor",
                            PS_link = "simple",
                            treatment_effect = 10,
                            alpha_PS = 5,
                            alpha_outcome = 0,
                            v_range = c(0,0),
                            e_range = c(0,0),
                            return_Matrix = FALSE) {
  if (X_dist == "normal_cor")   {
    Sigma = genPositiveDefMat(dim = n_X)$Sigma
    X = mvrnorm(n = n_obs, rep(0, n_X), Sigma)}
  else if(X_dist == "normal_no_cor")  {
    X = mvrnorm(n = n_obs, mu = rep(0, n_X), Sigma = diag(n_X))}
  else if(X_dist == "unif_no_cor")  {
    X = matrix(runif(n_obs), ncol=n_X)}
  else if(X_dist == "poisson_no_cor") {
    X = matrix(rpois(n_obs, lambda = 4), ncol=n_X)}
  v = runif(n_obs, min = v_range[1], max = v_range[2])
  e = runif(n_obs, min = e_range[1], max = e_range[2])
  dataset = data.frame(X)
  colnames(dataset) = c(paste0("X", 1:n_X))
  
  if (PS_link == "simple"){
    PS_unscaled = rowSums(alpha_PS * X) + v}
  else if(PS_link == "nonLinear"){
    intermediate = rowSums(alpha_PS *X)
    m = mean(intermediate)
    sd = sd(intermediate)
    PS_unscaled <- ifelse(intermediate>m, 
                          intermediate + 0.5 * sd, intermediate - 0.5 * sd)
  }
  
  PS_scaled = rescale(PS_unscaled, to = c(0,1))
  dataset["PS_scaled"] <- PS_scaled
  dataset["T"] <- rbinom(n_obs, 1, PS_scaled)
  
  dataset["Y"] <- treatment_effect * dataset["T"] + rowSums(alpha_outcome * X) + e
  dataset["T"] <- as.factor(dataset[["T"]])
  if (return_Matrix) {return(as.matrix(dataset))} else {return(dataset)}
}


###
# Generate propensity score by using link function (only used in univariat case)
### 

PS_fun <- function(alpha = 1, # Determines shift in PS link function in treated population 
                   beta = 0.6, # Determines proportion of X going into PS
                   gamma = 0, # Determines share of noise that goes into PS
                   noise_sd = 1, # Determines std dev of noise
                   X) {
  noise = rnorm(n = 1, mean = 0, sd = noise_sd)
  ps = alpha + beta*X + gamma * noise
  return(ps)
}
