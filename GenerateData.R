#Load  required packages (need to be installed beforehand)
packages <- c("MASS", "scales", "clusterGeneration")
lapply(packages, require, character.only = TRUE)


###
# Main function for creating datasets
###
#Receives parameters that are then passed on to functions for 
# (1) creating the matrix of covariates
# (2) calculating the propensity score based on these covariates
# (3) calculating the outcome based on covariates and treatment
#Default parameters are set for univariate DGP in linear setting from Milliment & Tchernis (2009) paper
#(Some of the parameters might be "legace" parameters in that they are not used in any of the final settings I am using)
#Creates dataset with following variables: X, Y, T, PS(true), Constant vector
gen_DS_modular <- function(n_obs = 1000,
                           X_dim = 1,
                           min_X = 0,
                           max_X = 1,
                           X_impact_share_PS = 1,
                           X_impact_shift_PS = 0,
                           PS_link = "logit",
                           PS_formula = "linear",
                           X_impact_share_Y = 1,
                           X_impact_shift_Y = 0,
                           adjust_alpha_Y = "X_dim",
                           adjust_standard_deviation_Y = "X_dim") {
  #Create covariates
  if(X_dim == "correlated") {
    X = gen_X_cor(n_obs = n_obs)
  } else if (is.numeric(X_dim)){
    X = gen_X(n = X_dim,
              n_obs = n_obs,
              min = min_X,
              max = max_X)
  }
  
  #Calculate propensity scores
  PS <- gen_PS(X = X,
               impact_share = X_impact_share_PS,
               impact_shift = X_impact_shift_PS,
               impact_formula = PS_formula,
               link_type = PS_link)
  
  #Draw treatment from binomial distribution with propabilites from calculated propensity score
  T = rbinom(n = n_obs, size = 1, prob = PS)
  
  #Generate outcomes, based on X and T
  Y = gen_Y(X = X, T = T, 
            impact_share = X_impact_share_Y, 
            impact_shift = X_impact_shift_Y,
            adjust_alpha = adjust_alpha_Y,
            adjust_standard_deviation = adjust_standard_deviation_Y)
  
  #Combine variables in one dataset
  dataset = data.frame(X,PS,T,Y)
  
  #Add vector of constant that is required for some models
  dataset["OneVector"] = 1
  
  dataset$T <- as.factor(dataset$T)
  
  return(dataset)
}


###
# Main function for drawing covariates
###
#Create covariates, (by default drawn from uniform distribution)
# n = number of covariates
# n_obs = number of rows in the data
# min_X / max_X = minimum / maximum of uniform distribution values are drawn from
gen_X <- function(n = 1, n_obs = 1000, min = 0, max = 1) {
  X = matrix(runif(n = n_obs * n, min = min, max = max), nrow = n_obs)
  return(X)
}

gen_X_cor <- function(n=3, n_obs = 1000){
  sigma <- matrix(c(1, 0.3, 0.1,
                    0.3, 1, 0.5,
                    0.1, 0.5, 1), 3, 3, byrow =TRUE)
  X_normalDist <- mvrnorm(n = n_obs, mu = rep(0,3), Sigma = sigma)
  X_uniformDist <- pnorm(X_normalDist)
  return(matrix(X_uniformDist, nrow = n_obs))
}

###
#Function to calculate PS based on X
###
# impact_share determines number of covariates that have an impact on the PS
# link_type determines the link function used to transform the "raw" PS 
gen_PS <- function(X, 
                   impact_share = 1,
                   impact_shift = 0,
                   impact_formula = "linear", 
                   link_type = "logit"){
  #Subset columns of covariates with impact on the PS
  N_shift <- ceiling(ncol(X) * impact_shift) #Share of vars that do not have an impact (from left)
  N_treat <- ceiling(ncol(X) * impact_share) #No of X with impact
  X_impact <- as.matrix(X[,(1+N_shift):(N_treat+N_shift)], ncol = N_treat)
  
  #Link from X to raw value of PS that will (potentially) transformed by logit/probit link function in next step
  if (impact_formula == "flat"){PS_raw <- rep(0, times = nrow(X))
  } else if (impact_formula == "linear"){
    PS_raw <- 1/N_treat* rowSums(-3 + 6 * X_impact) #Stimmt mit paper überein(-3 wird auf jede Spalte angewandt)
  } else if (impact_formula == "quadraticSymmetric"){
    PS_raw <- (1/N_treat)*rowSums(2.5 - (2.5*(1-2*X_impact))^2)
  } else if (impact_formula == "quadraticNonSymmetric"){
    PS_raw <-(1/N_treat)*rowSums(2.5 - (2.5*(1-1.5*X_impact))^2) #Stimmt auch mit paper überein
  } else if (impact_formula == "fourthDegree"){
    PS_raw <- (1/N_treat)*(-2.5 + rowSums(4.5 * X_impact^4))
    #Tested other specification, that did not work very well: 1:(1/N_treat)*rowSums(-2.5 + 4.5 * X_impact^4); 2:-2.5 + rowSums(4.5*(1/N_treat)*X_impact^4)
  } else if (impact_formula == "peakSymmetric"){
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    PS_raw <- ifelse(thresshold_value < 0.5, 
                     0.05 + 1.8*(rowSums(X_impact)) * 1/N_treat, 
                     1.85 - 1.8*(rowSums(X_impact)) * 1/N_treat)
  } else if (impact_formula == "peakNonSymmetric") {
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    if(ncol(X_impact)>1){
      percentiles <- quantile(thresshold_value, 0.8)} else{percentiles = c(0.8)}
    normalizer <- ifelse(percentiles>1,N_treat/percentiles[[1]],N_treat)
    PS_raw <- ifelse(thresshold_value<percentiles[[1]],
                     0.05 + 1.125*(rowSums(X_impact)) *1/normalizer,
                     4.55 - 4.5*(rowSums(X_impact)) * 1/(normalizer))
  } else if (impact_formula == "stepMonotonic"){
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    if(ncol(X_impact)>1){
      percentiles <- quantile(thresshold_value, c(0.33,0.67))} else{percentiles <- c(0.33,0.67)}
    PS_raw <- ifelse(thresshold_value<percentiles[[1]], 0.33, 
                     ifelse(thresshold_value>percentiles[[2]], 0.67, 0.5))
  } else if (impact_formula == "stepNonMonotonic"){
    thresshold_value <- rowSums(X_impact) - N_treat * 0.5 + 0.5
    if(ncol(X_impact)>1){
      percentiles <- quantile(thresshold_value, c(0.33,0.67))} else {percentiles <- c(0.33,0.67)}
    PS_raw <- ifelse(thresshold_value<percentiles[[1]], 0.5, 
                     ifelse(thresshold_value>percentiles[[2]], 0.1, 0.9))}
  
  #Transforming from raw values to propensity scores
  if (link_type == "raw") {PS <- PS_raw
  } else if (link_type == "logit"){
    PS = exp(PS_raw)/(exp(PS_raw) + 1)
  } else if (link_type == "probit") {
    PS = unlist(lapply(PS_raw, probit_function))
  }
  
  #Normalize PS score in case there are values <0, or >1
  PS <- ifelse(PS>1,1,ifelse(PS<0,0,PS))
  
  return(PS)
}

###
# Main outcome function
###
# X and T are inputs
# X_impact_share determines share of variables with an impact on the outcome
# adjust_alpha normalizes the impact of X on Y by 1/adjust_alpha to make the function flexible for high-dimensional settings
#   (usually default normalizes by number of covariates)

gen_Y <- function(X, T, 
                  impact_share = 1, 
                  impact_shift = 0,
                  adjust_alpha = "X_dim",
                  adjust_standard_deviation = "X_dim") {
  
  
  
  #Determine number of covariates that have an impact on the outcome 
  N_shift <- ceiling(ncol(X) * impact_shift) #Share of vars that do not have an impact (from left)
  N_treat <- ceiling(ncol(X) * impact_share) #No of X with impact
  X_impact <- as.matrix(X[,(1+N_shift):(N_treat+N_shift)], ncol = N_treat)
  
  #Adjust factor by which X as impact on Y directly and in interaction with T
  if (adjust_alpha == "X_dim"){
    alpha = 1/N_treat
    } else if (is.numeric(adjust_alpha)){
      alpha = 1/adjust_alpha
    }
  
  #Adjusting standard deviation of error term in outcome equation to numbers of vars having an impact on Y
  if (adjust_standard_deviation == "X_dim"){
    standard_deviation = 0.5 * N_treat
  } else if (is.numeric(adjust_standard_deviation)){
    standard_deviation = 1 * adjust_standard_deviation
  }
  
  #Create error term
  error = rnorm(n =  nrow(X), mean =  0, sd = standard_deviation) #Changed from 0.25 to 0.5
  
  #Outcome formula as specified in Millimet & Tchernis paper
  #but multiplying with alpha to normalize impact on Y
  Y = 2.54 - 
    (1.67 * T) - 
    2.47 * rowSums(X_impact) * alpha + 
    1.96 * T * rowSums(X_impact) * alpha + 
    error
  return(Y)
}

###
# PS probit link function
# Transform raw values to PS values between 0 and 1 with the probit link function
# (seems to be wrong)
###
probit_function <- function(PS_value){
  integrand = function(x) {exp(-0.5*x^2)}
  PS = 1/((2*pi)^0.5)*integrate(integrand, lower = -Inf, upper = PS_value)$value
  PS[PS>1] = 1 #For some specific values (e.g.) 245448, the function returns a value > 1
  PS[PS<0] = 0 #Included this just to make sure
  return(PS)}

