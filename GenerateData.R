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
                            Y_link = "simple",
                            PS_estimation = "simple",
                            treatment_assignment = "simple",
                            treatment_effect = 10,
                            alpha_PS = 5,
                            alpha_outcome = 0,
                            v_range = c(0,0),
                            e_range = c(0,0),
                            alpha_treat = 0.3, #used in special case only
                            lambda_treat = 1, #used in special case only
                            share_treated = 0.3, #used in special case only
                            return_Matrix = FALSE) {
  if (X_dist == "normal_cor")   {
    Sigma = genPositiveDefMat(dim = n_X)$Sigma
    X = mvrnorm(n = n_obs, rep(0, n_X), Sigma)}
  else if(X_dist == "normal_no_cor")  {
    X = mvrnorm(n = n_obs, mu = rep(0, n_X), Sigma = diag(n_X))}
  else if(X_dist == "unif_no_cor")  {
    X = matrix(runif(n_obs), nrow = n_obs, ncol=n_X)}
  else if(X_dist == "poisson_no_cor") {
    X = matrix(rpois(n_obs, lambda = 4), nrow = n_obs, ncol=n_X)}
  else if(X_dist == "two_norm_dist"){
    n_treated = round(share_treated * n_obs)
    n_Ntreated = n_obs - n_treated
    Sigma = genPositiveDefMat(dim = n_X)$Sigma
    X_treated = mvrnorm(n = n_treated, rep(3, n_X), Sigma)
    X_Ntreated = mvrnorm(n = n_Ntreated, rep(0, n_X), Sigma)
    X = rbind(X_treated, X_Ntreated)
  }
  v = runif(n_obs, min = v_range[1], max = v_range[2])
  e = runif(n_obs, min = e_range[1], max = e_range[2])
  dataset = data.frame(X)
  print(dim(dataset))
  colnames(dataset) = c(paste0("X", 1:n_X))
  
  if (PS_estimation == "simple"){
    PS_score_raw <- rowSums(alpha_PS * X) + v}
  else if(PS_estimation == "many_irrelevant_X") {
    coefficients <- sample(c(1,0), size = n_X, replace = T, prob = c(0.2,0.8)) * alpha_PS
    PS_score_raw <- X %*% coefficients
  }
  
  if (PS_link == "simple"){
    PS_unscaled = PS_score_raw}
  else if(PS_link == "nonLinear_1"){
    m = mean(PS_score_raw)
    sd = sd(PS_score_raw)
    PS_unscaled <- ifelse(PS_score_raw>m, 
                          PS_score_raw + 0.3 * sd, PS_score_raw - 0.3 * sd)
  }
  
  #Bring PS-score to lie between 0 and 1
  PS_scaled = rescale(PS_unscaled, to = c(0,1))
  dataset["PS_scaled"] <- PS_scaled
  
  #Treatment assignment
  if (treatment_assignment == "simple") {
    dataset["T"] <- rbinom(n_obs, 1, PS_scaled)}
  else if (treatment_assignment == "lechner"){
    beta_treat = round(runif(n_X, min = -3, max = 3))
    indicator <- lambda_treat * (X%*% beta_treat) + alpha_treat + rnorm(n = n_obs, mean = 0, sd = 1)
    #beta_treat in paper are parameters which have been estimated from sample
    #alpha_treat is there to vary share of treated and non-treated in sample
    #lambda determines degree of selection
    dataset["T"] <- ifelse(indicator > 0, 1, 0)
  }
  
  if (Y_link == "simple") {
    dataset["Y"] <- treatment_effect * dataset["T"] + rowSums(alpha_outcome * X) + e}
  else if (Y_link == "het_TE"){
    het_TE <- sample(c(treatment_effect - 0.5*treatment_effect,
                       treatment_effect + 0.5*treatment_effect), 
                     size = n_obs, replace = TRUE)
    dataset["Y"] <- het_TE * dataset["T"] + rowSums(alpha_outcome * X) + e
  }
  
  dataset["T"] <- as.factor(dataset[["T"]])
  if (return_Matrix) {return(as.matrix(dataset))} else {return(dataset)}
}

genDS_MV_varying_cov <- function(n_obs = 500,
                                 treatment_effect = 10,
                                 var = 0.5,
                                 alpha_PS = 0.3,
                                 alpha_outcome = 0,
                                 e_upper = 0,
                                 PS_shift = 0,
                                 return_Matrix = FALSE) {
  sigma_var <- matrix(c(1, var, 0,
                        var, 1, 0,
                        0, 0, 1), 3, 3, byrow =TRUE)
  X = mvrnorm(n = n_obs, mu = rep(0,3), Sigma = sigma_var)
  e = runif(n_obs, min = 0, max = e_upper)
  dataset = data.frame(X)
  colnames(dataset) = c(paste0("X", 1:3))
  
  PS_score_raw <- rowSums(alpha_PS * X) #H?here Werte f?hren zu extremerer Verteilung von PS_score
  PS_scaled <- exp(PS_score_raw)/(1+exp(PS_score_raw)) + PS_shift #Function liegt immer zwischen 0 und 1
  #Higher values in X (pos&neg) and alpha_PS lead to more very high and low prop. scores, and
  #hence also to less overlap
  #Bring PS-score to lie between 0 and 1
  dataset["PS_scaled"] <- PS_scaled
  
  #Treatment assignment
  dataset["T"] <- rbinom(n_obs, 1, PS_scaled)
  
  dataset["Y"] <- treatment_effect * dataset["T"] + rowSums(alpha_outcome * X) + e
  
  dataset["T"] <- as.factor(dataset[["T"]])
  if (return_Matrix) {return(as.matrix(dataset))} else {return(dataset)}
}

genDS_1 <- function(n_obs = 500,
                    n_X = 3,
                    x_y_not_unif = FALSE,
                    colinearity = FALSE,
                    covar = 0,             
                    alpha_PS = 0.3,
                    alpha_outcome = 0.5,
                    alpha_treat = 0,
                    lambda_treat = 0,
                    TA_lechner = FALSE,
                    Y_error = FALSE,
                    PS_error = FALSE
) {
  if (colinearity) {
    if(n_X==3){
      sigma <- matrix(c(1, covar, 0,
                        covar, 1, 0,
                        0, 0, 1), 3, 3, byrow =TRUE)}
    else{
      sigma = genPositiveDefMat(dim = n_X)$Sigma}
  }
  else {
    sigma <- diag(n_X)
  }
  X = mvrnorm(n = n_obs, mu = rep(0,n_X), Sigma = sigma)
  dataset = data.frame(X)
  colnames(dataset) = c(paste0("X", 1:n_X))
  
  if(PS_error) {v = c(-1,1)} else {v = c(0,0)}
  v = runif(n_obs, min = v[1], max = v[2])
  if(Y_error) {e = c(-1,1)} else {e = c(0,0)}
  e = runif(n_obs, min = e[1], max = e[2])
  if (x_y_not_unif){alpha_outcome = round(rnorm(n_X), digits = 1)}
  
  PS_score_raw <- rowSums(alpha_PS * X) + v #Could later make it non-uniform
  PS_scaled <- exp(PS_score_raw)/(1+exp(PS_score_raw)) 
  dataset["PS_scaled"] <- PS_scaled
  
  #Treatment assignment
  dataset["T"] <- rbinom(n_obs, 1, PS_scaled)
  if (TA_lechner){
    beta_treat = round(runif(n_X, min = -3, max = 3))
    indicator <- lambda_treat * (X%*% beta_treat) + alpha_treat + rnorm(n = n_obs, mean = 0, sd = 1)
    dataset["T"] <- ifelse(indicator > 0, 1, 0)}
  
  #Calculate outcome
  dataset["Y"] <- 10 * dataset["T"] + rowSums(alpha_outcome * X) + e
  
  #Transformation for models
  dataset["T"] <- as.factor(dataset[["T"]])
  return(dataset)
}

genDS_high_dim <- function(n_obs = 1000,
                           n_X = 100,
                           rescaleLim = 2,
                           PS_error = TRUE,
                           Y_error = TRUE,
                           lin_comb = FALSE,
                           high_cov = FALSE,
                           PS_link = "simple",
                           Y_link = "simple",
                           treatment_effect = 10) {
  ###
  #Creating X data
  ###
  X = mvrnorm(n_obs, mu = rep(0,n_X), Sigma = diag(n_X))
  if (lin_comb) {
    iter = 1
    grid = c(0, 5, 10, 15)
    num_lin_comb = length(grid)
    X_lin_comb = matrix(nrow = n_obs, ncol = num_lin_comb)
    for(i in grid) {
      X_lin_comb[,iter] <- reg_dependent(X[,(1+i):(5+i)]) + runif(n_obs, min = 0.3, max = 0.3)
      #Watch out if adding variables before
      iter  = iter + 1
    }
    X = cbind(X_lin_comb,X)
  }
  if (high_cov){
    X_high_cov = X_high_covar(n_obs = n_obs)
    X = cbind(X_high_cov, X)
  }
  dataset = data.frame(X)
  # colnames(X) = c(paste0("highCov", 1:3),
  #                 paste0("linComb",1:num_lin_comb),
  #                 paste0("X", 1:n_X))
  ###
  # Propensity link function
  ###
  if (PS_error) {ps_error <- runif(n_obs, min = -1, max = 1)} else {ps_error = rep(0, n_obs)}
  
  if (PS_link == "simple") {
    PS_score_raw <- rowSums(0.1*X) + ps_error
  }
  if (PS_link == "polynomial") {
    PS_score_raw <- rowSums(apply(X, MARGIN = 2, reg_polyn)) + ps_error
  }
  if (PS_link == "kink") {
    PS_score_raw <- rowSums(apply(X, MARGIN = 2, reg_kink)) + ps_error
  }
  # if (PS_link == "interaction") {
  #   PS_score_raw <- reg_interA(X)} + ps_error
  if (PS_link == "random") {
    PS_score_raw <- reg_rand(X) + ps_error
  }
  
  #Tranforming the score
  PS_score_raw <- rescale(PS_score_raw, to = c(-rescaleLim, rescaleLim))
  summary(PS_score_raw)
  PS_scaled <- exp(PS_score_raw)/(1+exp(PS_score_raw))
  summary(PS_scaled)
  dataset["PS_scaled"] <- PS_scaled
  
  ###
  # Linking to treatment
  ###
  dataset["T"] <- rbinom(n_obs, 1, PS_scaled)
  summary(dataset["T"])
  
  ###
  # Outcome modeling
  ###
  if (Y_error) {y_error <- runif(n_obs, min = -1, max = 1)} else {y_error = rep(0, n_obs)}
  
  if (Y_link == "simple") {
    dataset["Y"] <- treatment_effect * dataset["T"] + rowSums(0.1 * X) + y_error }
  if (Y_link == "polynomial") {
    dataset["Y"] <- treatment_effect * dataset["T"] + 
      rowSums(apply(X, MARGIN = 2, reg_polyn)) + y_error
  }
  if (Y_link == "kink") {
    dataset["Y"] <- treatment_effect * dataset["T"] + 
      rowSums(apply(X, MARGIN = 2, reg_kink)) + y_error
  }
  # if (Y_link == "interaction") {
  #   dataset["Y"] <- treatment_effect * dataset["T"] + 
  #reg_interA(X)} + y_error
  if (Y_link == "random") {
    dataset["Y"] <- treatment_effect * dataset["T"] + 
      reg_rand(X) + y_error
  }
  #Transformation for models
  dataset["T"] <- as.factor(dataset[["T"]])
  return(dataset)
}

#New set-up for new simple set up
gen_multiple_easy_DS <- function(n_obs = 500,
                                 X_dim = 1,
                                 min_X = -1,
                                 max_X = 1,
                                 cor_X = 0,
                                 alpha_PS = -0.2,
                                 beta_PS = 1,
                                 PS_link = "logit",
                                 alpha_outcome = 1,
                                 Y_error = FALSE,
                                 treatment_effect = 1) {
  if (X_dim == 1){
    X = runif(n = n_obs, min = min_X, max = max_X)
  } else {
    x1 = runif(n_obs, min = min_X, max = max_X)
    x2 = runif(n_obs, min = min_X, max = max_X)
    X_1 = x1
    X_2 = cor_X * x1 + (1-cor_X) * x2
    X = cbind(X_1, X_2)
  }
  one_vector = rep(x = 1, n_obs) #Needed for glmnet to work in case of X_dim == 1
  dataset = data.frame(X, one_vector)
  colnames(dataset) = c(paste0("X", 1:(ncol(dataset)-1)), "OneVector")
  
  if (X_dim == 1) {PS_raw = beta_PS * X + alpha_PS
  } else {
    PS_raw = rowSums(beta_PS * X) + alpha_PS
    #Here, could divide first component by no of X to normalize
  }
  if (PS_link == "logit"){
    PS_scaled = exp(PS_raw)/(exp(PS_raw) + 1)
  }
  else if (PS_link == "probit") {
    PS_scaled = unlist(lapply(PS_raw, probit_function))
  }
  dataset["PS_scaled"] = PS_scaled
  
  dataset["T"] = rbinom(n_obs, 1, prob = PS_scaled)
  
  if (sum(is.na(dataset$T)) > 0) {browser()}
  
  if(Y_error) {
    y_error = runif(n_obs, min = -1, max = 1)} 
  else {
    y_error = runif(n_obs, min = 0, max = 0)}
  dataset["Y"] = treatment_effect * dataset["T"] + alpha_outcome * X + y_error
  
  dataset["T"] <- as.factor(dataset[["T"]])
  return(dataset)
}


###
# Implement new elements and functions for DGP
###

###Variable creation
#One var as linear combination of other vars
reg_dependent <- function(x, non_linear = FALSE, include_error = TRUE) {
  #Takes matrix x values as input
  params <- round(runif(ncol(x), -1, 1), digits = 1)
  x_new <- x%*%params 
  if (include_error) {x_new + runif(n = nrow(x_new), min = -1, max = 1)}
  return(x_new)
}

#Create matrix of 3 vars with very high degree of covariance
X_high_covar <- function(n_obs) {
  sigma <- matrix(c(1, 0.95, 095,
                    0.95, 1, 0.8,
                    0.85, 0.8, 1), 3, 3, byrow =TRUE)
  X_high_covar <- mvrnorm(n = n_obs, mu = rep(0,3), Sigma = sigma)
}

###Links from X to D & Y
#Regression curve as parameter
reg_polyn <- function(x, a = 0.8, b = 0.25, c = -0.1, d = 0.2) {
  #Usage: apply(X, MARGIN = 2, reg_polyn)
  y = a*x+b*x^2+c*x^3+d*x^4
  return(y)}

#Kink and/or Jump in parameter
reg_kink <- function(x){
  param <- ifelse(x>0, ifelse(x>1, -1, 1), 0.5)
  result <- x * param
  return(result)
}

#Interaction term in parameter
reg_interA <- function(x){
  #Needs matrix of (at least) two columns
  param_1 <- ifelse(x[,1]>1 & x[,2]<1, 1, -1)
  param_2 <- ifelse(x[,1]>0 & x[,2]<0, -1, 1)
  result_1 <- x[,1] * param_1
  result_2 <- x[,2] * param_2
  y = result_1 + result_2
  return(y)
}

#Random impact
reg_rand <- function(x, min = -3, max = 3) {
  parameters <- matrix(runif(n = nrow(x)*ncol(x), min = min, max = max), nrow = nrow(x))
  result = rowSums(parameters * x)
  return(result)
}

###PS score link function
#Maybe improve upon the lechner approach

#Find other posible links 


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

probit_function <- function(PS_value){
  integrand = function(x) {exp(-0.5*x^2)}
  PS_scaled = 1/((2*pi)^0.5)*integrate(integrand, lower = -Inf, upper = PS_value)$value
  PS_scaled[PS_scaled>1] = 1 #For some specific values (e.g.) 245448, the function returns a value > 1
  PS_scaled[PS_scaled<0] = 0 #Included this just to make sure
  return(PS_scaled)}
