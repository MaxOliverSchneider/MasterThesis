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
  PS = rescale(PS_unscaled, to = c(0,1))
  dataset["PS"] <- PS
  dataset["T"] <- rbinom(n_obs, 1, PS)
  
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
  PS = rescale(PS_unscaled, to = c(0,1))
  dataset["PS"] <- PS
  dataset["T"] <- rbinom(n_obs, 1, PS)
  
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
  PS = rescale(PS_unscaled, to = c(0,1))
  dataset["PS"] <- PS
  
  #Treatment assignment
  if (treatment_assignment == "simple") {
    dataset["T"] <- rbinom(n_obs, 1, PS)}
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
  PS <- exp(PS_score_raw)/(1+exp(PS_score_raw)) + PS_shift #Function liegt immer zwischen 0 und 1
  #Higher values in X (pos&neg) and alpha_PS lead to more very high and low prop. scores, and
  #hence also to less overlap
  #Bring PS-score to lie between 0 and 1
  dataset["PS"] <- PS
  
  #Treatment assignment
  dataset["T"] <- rbinom(n_obs, 1, PS)
  
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
  PS <- exp(PS_score_raw)/(1+exp(PS_score_raw)) 
  dataset["PS"] <- PS
  
  #Treatment assignment
  dataset["T"] <- rbinom(n_obs, 1, PS)
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
  PS <- exp(PS_score_raw)/(1+exp(PS_score_raw))
  summary(PS)
  dataset["PS"] <- PS
  
  ###
  # Linking to treatment
  ###
  dataset["T"] <- rbinom(n_obs, 1, PS)
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
    PS = exp(PS_raw)/(exp(PS_raw) + 1)
  }
  else if (PS_link == "probit") {
    PS = unlist(lapply(PS_raw, probit_function))
  }
  dataset["PS"] = PS
  
  dataset["T"] = rbinom(n_obs, 1, prob = PS)
  
  if(Y_error) {
    y_error = runif(n_obs, min = -1, max = 1)} 
  else {
    y_error = runif(n_obs, min = 0, max = 0)}
  dataset["Y"] = treatment_effect * dataset["T"] + alpha_outcome * X + y_error
  
  dataset["T"] <- as.factor(dataset[["T"]])
  return(dataset)
}

gen_DS_modular <- function(n_obs = 500,
                           X_dim = 1,
                           X_dummy_share = 0,
                           X_prob_dummy = 0.5,
                           X_factor_share = 0,
                           X_covar = 0,
                           min_X = -1,
                           max_X = 1,
                           alpha_PS = 1,
                           X_impact_share_PS = 1,
                           X_impact_share_within = 1, 
                           beta_PS = 1,
                           beta_adjust_power_PS = 0,
                           alpha_adjust_power_PS = 0,
                           adjust_beta_PS = FALSE,
                           PS_function = "paper",
                           PS_link = "logit",
                           PS_impact = "linear",
                           to_interact_PS = list(c(1,2)),
                           PS_error = FALSE,
                           Y_impact = "linear",
                           X_impact_share_Y = 1,
                           X_impact_shift_percent_Y = 0,
                           alpha_outcome = 1,
                           adjust_alpha_Y = FALSE, 
                           alpha_adjust_power_Y = 0.5,
                           Y_error = FALSE,
                           min_Y_error = -1,
                           max_Y_error = 1,
                           treatment_effect = 1) {
  #Create variables
  X = gen_X(n = X_dim, n_obs = n_obs, min = min_X, max = max_X, covar = X_covar,
            dummy_share = X_dummy_share, prob_dummy = X_prob_dummy,
            factor_share = X_factor_share)
  if (PS_function == "paper") {
    PS = gen_PS_paper(X = X, impact_share = X_impact_share_PS, beta = beta_PS, beta_adjust_power = beta_adjust_power_PS,
                      alpha = alpha_PS, alpha_adjust_power = alpha_adjust_power_PS, impact_formula = PS_impact, link_type = PS_link)
  } else if (PS_function == "paperold") {
    PS = gen_PS_paper_old(X = X,beta = beta_PS, impact_formula = PS_impact, link_type = PS_link)
  } else if (PS_function == "highdim") {
    PS = gen_PS_highDim(X = X, impact_share = X_impact_share_PS, beta = beta_PS, beta_adjust_power = adjust_beta_PS,
                        alpha = alpha_PS, alpha_adjust_power = alpha_adjust_power_PS)
  } else {
  PS = gen_PS(X = X, link_type = PS_link, X_impact_share = X_impact_share_PS, impact_formula = PS_impact,
              error = PS_error, alpha_PS = alpha_PS, beta_PS = beta_PS, adjust_beta = adjust_beta_PS, 
              beta_adjust_power = beta_adjust_power_PS, to_interact = to_interact_PS, impact_share_within = X_impact_share_within)}
  T = rbinom(n = n_obs, size = 1, prob = PS)
  Y = gen_Y(X = X, T = T, X_impact_share = X_impact_share_Y, X_impact_shift_percent = X_impact_shift_percent_Y, 
            error = Y_error, min_error = min_Y_error, max_error = max_Y_error, 
            treatment_effect = treatment_effect, alpha_outcome = alpha_outcome, 
            adjust_alpha = adjust_alpha_Y, impact_formula = Y_impact, alpha_adjust_power = alpha_adjust_power_Y)
  #Create dataset
  dataset = data.frame(X,PS,T,Y)
  dataset["OneVector"] = 1
  dataset$T <- as.factor(dataset$T)
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
reg_kink <- function(x, a = -1, b = 1, c = 0.5){
  param <- ifelse(x>0, ifelse(x>0.5, a, b), c)
  result <- x * param
  return(result)
}

#Interaction term in parameter
reg_interA <- function(x){
  #Needs matrix of (at least) two columns
  param_1 <- ifelse(x[,1]>0.5 & x[,2]<0.5, 1, -1)
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
  PS = 1/((2*pi)^0.5)*integrate(integrand, lower = -Inf, upper = PS_value)$value
  PS[PS>1] = 1 #For some specific values (e.g.) 245448, the function returns a value > 1
  PS[PS<0] = 0 #Included this just to make sure
  return(PS)}

###
# Reusablefunctions for DGP
###

gen_X <- function(n = 1, n_obs = 500, covar = 0, min = -1, max = 1,
                  dummy_share = 0, X_dummy_shift_percent = 0, prob_dummy = 0.5,
                  factor_share = 0) {
  if (n == 1) {
    X = matrix(runif(n = n_obs * n, min = min, max = max), nrow = n_obs)
  } else {
    if((!covar==0) & (!(n%%2==0))) {print("If covar >0, n must be an even number")}
    #Two matrices with ncol = n and nrow = n_obs each
    X = matrix(runif(n = n_obs * n, min = min, max = max), nrow = n_obs)
    X2 = matrix(runif(n = n_obs * n, min = min, max = max), nrow = n_obs)
    #Indicators mark every second column, starting at first/second column
    indicator1 = seq(1, n, 2)
    indicator2 = seq(2, n, 2)
    #If covar > 0, every second column is (1-covar)*itself + covar * value of previous column
    #Thereby creating pairwise correlations of columns 1&2, 3&4 and so on
    if (covar > 0) {
    X[,indicator2] <- (1-covar) * X2[,indicator2] + covar * X[,indicator1]}
    
    #If required, include dummies
    #!Shift not implemented ye
    if(dummy_share > 0) {
      n_dummies <- ceiling(dummy_share * n)
      
      #shift_abs <- ceiling(n*X_dummy_shift_percent)
      X_dummy <- matrix(rbinom(n = n_obs * n_dummies, size = 1, prob = prob_dummy), 
                        nrow = n_obs)
      X[,1:n_dummies] <- X_dummy
    }
    if(factor_share > 0) {
      n_factors <- ceiling(factor_share*n)
      X_factor <- matrix(sample(c(-1, -0.3, 0.3, 1), size = n_factors * n_obs, replace = TRUE),
                         nrow = n_obs)
      X[,1:n_factors] <- X_factor
    }
  } 
  return(X)
}

gen_PS <- function(X, X_impact_share = 1, impact_formula = "linear", to_interact =list(c(1,2)),
                   link_type = "logit", error = FALSE, alpha_PS = 0, impact_share_within = 1,
                   beta_PS = 1, adjust_beta = FALSE, beta_adjust_power = 0.35, min_error = -1, max_error = 1) 
  #beta_adjust_power determines the degree to which beta is adjusted to the dimension of X in order to control dispersion of the PS score
  {
  if (ncol(X) == 1) {
    PS_raw = beta_PS * X + alpha_PS
    if (impact_formula == "quadratic") {PS_raw = 0.5 * (beta_PS * X + beta_PS * X^3) + alpha_PS} 
    else if (impact_formula == "non-linear") {PS_raw = reg_kink(X) * beta_PS + alpha_PS}
    if(!impact_formula %in%(c("linear", "quadratic", "non-linear"))) {print("Attention: PS impact formula not because ncol(X) = 1")}
  } else {
    #If required, adjust beta
    if(adjust_beta){
      adjust_factor = (ncol(X)*X_impact_share)^beta_adjust_power
      beta_PS = beta_PS/(adjust_factor)}
    
    #If 
    n_treat = ceiling(ncol(X)*X_impact_share)
    n_Ntreat = ncol(X)-n_treat
    treatment_indicator = c(rep(1, n_treat), rep(0, n_Ntreat))
    #treatment_indicator = sample(c(rep(1, n_treat), rep(0, n_Ntreat)), replace = FALSE) #Could be used if dist of var with and without impact should be random
    
    PS_raw = rowSums(beta_PS * t(t(X) * treatment_indicator)) + alpha_PS
    if (impact_formula == "quadratic") {
      PS_raw =  rowSums(beta_PS * t(t(X^2) * treatment_indicator))  + 
        alpha_PS}
    else if (impact_formula == "quadratic_2") {
      PS_raw = 0.25 * rowSums(beta_PS * t(t(X^3) * treatment_indicator) + 
                                beta_PS * t(t(X) * treatment_indicator)) + 
        alpha_PS}
    else if (impact_formula == "quadratic_3") {
      PS_raw = rowSums(beta_PS * t(t(X^5) * treatment_indicator) - 
                         beta_PS * t(t(X^3) * treatment_indicator) +
                         beta_PS * t(t(X) * treatment_indicator) + 
                         alpha_PS)
    }
    else if (impact_formula == "quadratic_4") {
      PS_raw = rowSums(beta_PS * t(t(X^7) * treatment_indicator) - 
                         beta_PS * t(t(X^5) * treatment_indicator) +
                         beta_PS * t(t(X^3) * treatment_indicator) + 
                         alpha_PS)
    }
    else if (impact_formula == "kink") {
      PS_raw = rowSums(beta_PS * t(t(reg_kink(X)) * treatment_indicator)) + 
        alpha_PS }
    else if (impact_formula == "kink_2") {
      PS_raw = rowSums(beta_PS * t(t(reg_kink(X, a = 1.5, b = -2, c = 0)) * treatment_indicator)) + 
        alpha_PS 
    }
    else if (impact_formula == "small_share"){
      to_treat <- ceiling(sum(treatment_indicator) * impact_share_within)
      treatments <- sample(c(
        rep(1,to_treat), rep(0,sum(treatment_indicator)-to_treat)), 
        replace = FALSE)
      treatment_indicator[1:sum(treatment_indicator)] <- treatments
      PS_raw = rowSums(beta_PS * t(t(X) * treatment_indicator)) + alpha_PS
    }
    #Here small impact share (randomly distributed vars with impact and impact strengths)
    else if (impact_formula == "interaction_mult"){
      #Might be problematic, since multiplication makes values smaller
      X_impact <- t(t(X) * treatment_indicator)
      interact_fun <- function(columns, X) {X[,columns[1]] * X[,columns[2]]}
      interacted_X <- lapply(to_interact, interact_fun, X_impact)
      interacted_X <- matrix(unlist(interacted_X), ncol = length(to_interact))
      #Would need to write somethink to include X that have not been interacted still
      PS_raw = rowSums(beta_PS * interacted_X) + alpha_PS
    }
    else if (impact_formula == "interaction_div"){
      #!!! Produces extreme values for PS score
      X_impact <- t(t(X) * treatment_indicator)
      interact_fun <- function(columns, X) {X[,columns[1]] / X[,columns[2]]}
      interacted_X <- lapply(to_interact, interact_fun, X_impact)
      interacted_X <- matrix(unlist(interacted_X), ncol = length(to_interact))
      #Would need to write somethink to include X that have not been interacted still
      PS_raw = rowSums(beta_PS * interacted_X) + alpha_PS
      #Should make sure values are not so extreme that NAs are produced
      PS_raw = ifelse(PS_raw > 100, 100, ifelse(PS_raw < -100, -100, PS_raw))
    }
    else if (impact_formula == "conditional_interact_1"){
      #Needs interaction lists of length 2
      X_impact <- t(t(X) * treatment_indicator)
      interact_fun <- function(columns, X, 
                               small_factor = 0.5, large_factor = 1.5,
                               small_threshhold = -1, large_thresshold = 0.15) {
        ifelse((X[,columns[1]]>small_threshhold & X[,columns[1]]<large_thresshold) & 
                 (X[,columns[2]]>small_threshhold & X[,columns[2]]<large_thresshold), 
               small_factor*X[,columns[1]]*X[,columns[2]],
               large_factor*X[,columns[1]]*X[,columns[2]])}
      #Warning, this only works if there are at least to pairs of interaction terms, otherwise list with each element being a column is returned
      interacted_X <- lapply(to_interact, interact_fun, X_impact)
      interacted_X <- matrix(unlist(interacted_X), ncol = length(to_interact))
      #Would need to write somethink to include X that have not been interacted still
      PS_raw = rowSums(beta_PS * interacted_X) + alpha_PS
    }
    else if (impact_formula == "conditional_interact_2"){
      #Needs interaction lists of length 2
      X_impact <- t(t(X) * treatment_indicator)
      interact_fun <- function(columns, X, 
                               small_factor = 0.5, large_factor = -1.5,
                               small_threshhold = -1, large_thresshold = 0) {
        ifelse((X[,columns[2]]>small_threshhold & X[,columns[2]]<large_thresshold), 
               small_factor*X[,columns[1]],
               large_factor*X[,columns[1]])}
      #Warning, this only works if there are at least to pairs of interaction terms, otherwise list with each element being a column is returned
      interacted_X <- lapply(to_interact, interact_fun, X_impact)
      interacted_X <- matrix(unlist(interacted_X), ncol = length(to_interact))
      #Would need to write somethink to include X that have not been interacted still
      PS_raw = rowSums(beta_PS * interacted_X) + alpha_PS}
    else if (impact_formula == "conditional_interact_3"){
      #Needs interaction lists of length 3
      X_impact <- t(t(X) * treatment_indicator)
      interact_fun <- function(columns, X, 
                               small_factor = 0.5, large_factor = -1.5,
                               small_threshhold = -1, large_thresshold = 0) {
        ifelse((X[,columns[3]]>small_threshhold & X[,columns[3]]<large_thresshold), 
               small_factor*X[,columns[1]]*X[,columns[2]],
               large_factor*X[,columns[1]]*X[,columns[2]])}
      #Warning, this only works if there are at least to pairs of interaction terms, otherwise list with each element being a column is returned
      interacted_X <- lapply(to_interact, interact_fun, X_impact)
      interacted_X <- matrix(unlist(interacted_X), ncol = length(to_interact))
      #Would need to write somethink to include X that have not been interacted still
      PS_raw = rowSums(beta_PS * interacted_X) + alpha_PS
    }
    else if (impact_formula == "range_assign"){
      #Function that returns specific value if X lies in specific range
      #Like 1 if -1<X-0.5, 2 if -0.5<X<0, ...
      n_cuts =3 
      threshhold = c(-0.4,0.5)
      X_impact <- t(t(X) * treatment_indicator)
      assign_fun <- function(X) {
        #impact <- as.numeric(paste(round(runif(n = n_cuts, min = -3, max = 3), digits = 0), round(runif(n = n_cuts, min = 0, max = 9), digits = 0), sep = "."))
        #Had to fix impact because the PS distribution was vaying too widely
        impact <- c(-2,0.3,1.2)
        ifelse(X<threshhold[1], impact[1], ifelse(X>threshhold[2], impact[2], impact[3]))
      }
      X_assigned <- apply(X_impact, 2, assign_fun)
      PS_raw = rowSums(beta_PS * X_assigned) + alpha_PS
    }
  }
  if (link_type == "logit"){
    PS = exp(PS_raw)/(exp(PS_raw) + 1)
  }
  else if (link_type == "probit") {
    PS = unlist(lapply(PS_raw, probit_function))
  }
  return(PS)
}

gen_PS_paper_old <- function(X, 
                         beta_PS = 1, 
                         impact_formula = "flat", 
                         link_type = "logit"
                         ){
  #First, I'll implement it for the univariate case
  if (impact_formula == "flat"){PS_raw <- rep(0, times = nrow(X))}
  if (impact_formula == "linear"){PS_raw <- (beta_PS/ncol(X)) * (-3 * ncol(X) + rowSums(6 * X))}
  if (impact_formula == "quadraticSymmetric"){PS_raw <- 2.5 - (2.5*(1-rowSums(2*X)))^2}
  if (impact_formula == "quadraticNonSymmetric"){PS_raw <- 2.5 - (2.5*(1-rowSums(1.5*X)))^2}
  if (impact_formula == "fourthDegree"){PS_raw <- -2.5 + rowSums(4.5 * X^4)}
  if (impact_formula == "peakSymmetric"){
    PS_raw <- ifelse(X<0.5, 0.05 + 1.8*X, 1.85 - 1.8*X)}
  if (impact_formula == "peakNonSymmetric") {
    PS_raw <- ifelse(X<0.8, 0.05 + 1.125*X, 4.55 - 4.5*X)}
  if (impact_formula == "stepMonotonic"){
    PS_raw <- ifelse(X<0.33, 0.33, ifelse(X>0.67, 0.67, 0.5))}
  if (impact_formula == "stepNonMonotonic"){
    PS_raw <- ifelse(X<0.33, 0.5, ifelse(X>0.67, 0.1, 0.9))}
  
  if (link_type == "raw") {PS <- PS_raw}
  if (link_type == "logit"){
    PS = exp(PS_raw)/(exp(PS_raw) + 1)
  }
  else if (link_type == "probit") {
    PS = unlist(lapply(PS_raw, probit_function))
  }
  return(PS)
}

gen_PS_paper <- function(X, 
                     beta = 1, 
                     beta_adjust_power = 0,
                     alpha = 1,
                     alpha_adjust_power = 0,
                     impact_share = 1, 
                     impact_formula = "flat", 
                     link_type = "logit"){
  
  N_treat <- ceiling(ncol(X) * impact_share) #No of X with impact
  beta <- beta/(N_treat^beta_adjust_power) #
  alpha <- alpha/(N_treat^alpha_adjust_power)
  X_impact <- matrix(X[,1:N_treat], ncol = N_treat)
  if (impact_formula == "flat"){PS_raw <- rep(0, times = nrow(X))}
  if (impact_formula == "linear"){PS_raw <- 1/N_treat* rowSums(-3 + 6 * X_impact)} 
  #Alpha*N_treat usually cancel out, but for now keep it, since might be useful for tweaking ...
    #PS_raw <- (beta_PS/ncol(X)) * (-3 * ncol(X) + rowSums(6 * X))}
  #PS_raw <- 2.5 - (2.5*(1-rowSums(2*X)))^2}
  if (impact_formula == "quadraticSymmetric"){PS_raw <- alpha * (1/N_treat)*rowSums(2.5 - (2.5*(1-2*beta*X_impact))^2)}
  if (impact_formula == "quadraticNonSymmetric"){PS_raw <- alpha * (1/N_treat)*rowSums(2.5 - (2.5*(1-1.5*beta*X_impact))^2) }
  # PS_raw <- -2.5 + rowSums(4.5 * X^4)
  if (impact_formula == "fourthDegree"){PS_raw <- alpha * (1/N_treat)*(-2.5 + rowSums(4.5 * beta * X_impact^4))}
  #Wouldnt it be better like this? PS_raw <- -2.5 + rowSums(4.5*(1/N_treat)*X_impact^4)
  if (impact_formula == "peakSymmetric"){
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    # PS_raw <- ifelse(thresshold_value < 0.5, 
    #                  0.05 + 1.8*(rowSums(X_impact) *beta - (N_treat-1) * alpha * 0.5), 
    #                  1.85 - 1.8*(rowSums(X_impact) *beta - (N_treat-1) * alpha * 0.5))
    # PS_raw <- ifelse(thresshold_value < 0.5, 
    #                  0.05 + 1.8*(rowSums(X_impact)-(1/N_treat-1)*0.5) * 1/N_treat, 
    #                  1.85 - 1.8*(rowSums(X_impact)-(1/N_treat-1)*0.5) * 1/N_treat)
    PS_raw <- ifelse(thresshold_value < 0.5, 
                     0.05 + 1.8*(rowSums(X_impact)) * 1/N_treat, 
                     1.85 - 1.8*(rowSums(X_impact)) * 1/N_treat)}
  #Need to really think through all formulas again, here subtracted 1 from N-treat, since we don't want to substract 0.5 in case of N_treat = 1
  #And this also holds for N-treat>1, we need to subtract one less *0.5
  
  if (impact_formula == "peakNonSymmetric") {
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    if(ncol(X_impact>1)){
    percentiles <- quantile(thresshold_value, 0.8)} else{percentiles = c(0.8)}
    normalizer <- ifelse(percentiles>1,N_treat/percentiles[[1]],N_treat)
    PS_raw <- ifelse(thresshold_value<percentiles[[1]],
                     0.05 + 1.125*(rowSums(X_impact)) *1/normalizer,
                     4.55 - 4.5*(rowSums(X_impact)) * 1/(normalizer))}
  #hist(PS_raw[thresshold_value>percentiles[[1]]])
  if (impact_formula == "stepMonotonic"){
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    if(ncol(X_impact>1)){
    percentiles <- quantile(thresshold_value, c(0.33,0.67))} else{percentiles <- c(0.33,0.67)}
    PS_raw <- ifelse(thresshold_value<percentiles[[1]], 0.33, 
                     ifelse(thresshold_value>percentiles[[2]], 0.67, 0.5))}
  if (impact_formula == "stepNonMonotonic"){
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    if(ncol(X_impact>1)){
    percentiles <- quantile(thresshold_value, c(0.33,0.67))} else {percentiles <- c(0.33,0.67)}
    PS_raw <- ifelse(thresshold_value<percentiles[[1]], 0.5, 
                     ifelse(thresshold_value>percentiles[[2]], 0.1, 0.9))}

  
  if (link_type == "raw") {PS <- PS_raw}
  if (link_type == "logit"){
    PS = exp(PS_raw)/(exp(PS_raw) + 1)
  }
  else if (link_type == "probit") {
    PS = unlist(lapply(PS_raw, probit_function))
  }
  #Normalize PS score in case there are values <0, or >1
  #Will need to look at distributions of ps scores in more detail
  PS <- ifelse(PS>1,1,ifelse(PS<0,0,PS))
  return(PS)
}



gen_PS_highDim <- function(X, impact_share, beta = 6, beta_adjust_power = 1, alpha = -3, alpha_adjust_power = 1){
  N_treat <- ceiling(ncol(X) * impact_share)
  beta <- beta/(N_treat^beta_adjust_power)
  alpha <- alpha/(N_treat^alpha_adjust_power)
  X_impact <- matrix(X[,1:N_treat], ncol = N_treat)
  PS_raw <- alpha * N_treat + beta * rowSums(X_impact)
  PS <- exp(PS_raw)/(exp(PS_raw)+1)
  return(PS)
}

gen_Y <- function(X, T, X_impact_share = 1, X_impact_shift_percent = 0, error = FALSE, 
                  min_error = -1, max_error = 1, treatment_effect = 1, alpha_outcome = 1, 
                  adjust_alpha = FALSE, impact_formula = "linear", alpha_adjust_power = 0.5) {
  if (ncol(X) > 1) {
  n_treat = ceiling(ncol(X)*X_impact_share)
  X_impact_shift_abs <- ceiling(X_impact_shift_percent * ncol(X))
  X_impact_shift_abs = 
    ifelse(X_impact_shift_abs > (ncol(X) - n_treat), (ncol(X)-n_treat), X_impact_shift_abs)
  n_Ntreat = ncol(X)-n_treat - X_impact_shift_abs #Used to make the two impact groups not overlap
  treatment_indicator = c(rep(0, X_impact_shift_abs), rep(1, n_treat), rep(0, n_Ntreat)) 
  } else {treatment_indicator = 1}
  
  #If required, adjust beta
  if(adjust_alpha){
    adjust_factor = (ncol(X)*X_impact_share)^alpha_adjust_power
    alpha_outcome = alpha_outcome/(adjust_factor)}
  
  if(error) {
    y_error = runif(length(T), min = min_error, max = max_error)
  } else {
    y_error = runif(length(T), min = 0, max = 0)}
  
  if (impact_formula == "linear") {
    Y = treatment_effect * T + rowSums(alpha_outcome * t(t(X)*treatment_indicator)) + y_error
  } else if (impact_formula == "quadratic") {
    Y = treatment_effect * T  + 0.25 * rowSums(alpha_outcome * t(t(X^3) * treatment_indicator) + 
                                                 alpha_outcome * t(t(X) * treatment_indicator)) + y_error
  } else if (impact_formula == "non-linear") {
    Y = treatment_effect * T + rowSums(alpha_outcome * t(t(reg_kink(X)) * treatment_indicator)) + y_error 
  } else if (impact_formula == "heterogeneous") {
    alpha_outcome <- ifelse(T, alpha_outcome, 0.5*alpha_outcome)
    Y = treatment_effect * T + alpha_outcome * rowSums(t(t(X)*treatment_indicator)) + y_error
  } else if (impact_formula == "paper") {
    Y = 2.54 - 
      (1.67 * T) - 
      (2.47 * rowSums(X)) + 
      (1.96 * T * rowSums(X)) + 
      rnorm(n =  nrow(X), mean =  0, sd = 0.25)
  } else if (impact_formula == "highdim") {
    N_treat = ceiling(ncol(X)*X_impact_share)
    Y = 2.54 - 
      (1.67 * T) - 
      (2.47 * rowSums(X)) * alpha_outcome + 
      (1.96 * T * rowSums(as.matrix(X[,1:N_treat]))) * alpha_outcome + 
      rnorm(n =  nrow(X), mean =  0, sd = 0.25)
  }
  return(Y)
}
