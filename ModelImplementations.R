# Load required packages
packages <- c("tidyverse","tidymodels", "glmnet", "workflows", "tune", "magrittr", 
              "mlr3", "mlr3learners", "data.table", "bayesreg", "keras", "DoubleML", "randomForest")
lapply(packages, require, character.only = TRUE)

###
# Implementation for lasso
###
# include_interactions = determines whether intercations of covariates should be used in regression
# Polynomials_vector = Degree of polynomials used
# Vars to exclude = Variables to be dropped from set of independent variables
# return_modelAndPS = Should model be returned in addition to propensity scores
# nfolds = number of folds in cross-validation of lambda
Lasso_PS_pred <- function(data, 
                          target_var = "T", 
                          include_interactions = FALSE,
                          polynomials_vector = NA,
                          vars_to_exclude = c("Y", "PS"), 
                          nfolds = 5,
                          return_modelAndPS = FALSE) {
  
  #Determine that should not be in array of independent variables
  drop_vars <- c(target_var, vars_to_exclude)
  
  #Save X without any transformations
  raw_X <- as.matrix(data[,!names(data) %in% drop_vars])
  
  #Select names of vars to be transformed (exclude constant vector here)
  #Lasso does not use the constant vector ("OneVector") at all
  regressors <- colnames(raw_X)[colnames(raw_X)!="OneVector"]
  
  X = raw_X
  
  #If vector of polynomials is specified, these polynomials are calculated and the resulting array overwrites X
  if (!is.na(polynomials_vector)) {
    #Create empty matrix
    polynomials <- matrix(nrow = nrow(raw_X), ncol = 0)
    #Calculate polynomials
    for (i in polynomials_vector){
      polynomials <- cbind(polynomials,raw_X[,regressors]^i)}
    colnames(polynomials) <- paste0(colnames(polynomials), "squared", rep(polynomials_vector, each = length(regressors)))
    #Over write X with polynomials specified
    X <- polynomials}
  
  #Calculate interactions if sppecified
  if (include_interactions & (ncol(raw_X) > 2)) {
    interactions <- model.matrix( ~.^2, data=data.frame(raw_X[,-ncol(raw_X)]))[,-c(1:ncol(raw_X))]
    X <- cbind(X, interactions)}
  
  X <- as.matrix(X)
  
  T <- as.matrix(data[,target_var])
  
  #Calculate lasso if there is either a polynomial of degree larger than one specified (sum(polynomials_vector))
  # OR no polynomial vector is specified, which means just main effects are included
  # This check needs to be done since the lasso is not estimated with only zero variance covariates
  if (sum(sum(polynomials_vector)>0 | is.na(polynomials_vector))>0) {
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = "binomial", nfolds = nfolds)
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
  
  #Add marker which kind of model is calculated, this is later needed in calculation of MISE and SUP
  marker <- "lasso/ridge"
  
  #Add number of polynomials used (also needed for calculation of metrics later)
  poly <- polynomials_vector
  
  #Calculate PS score predictions
  PS_pred_lasso <- as.vector(predict(object = log_lasso_reg, newx = X, type = "response"))
  #Add marker so that MISE and SUP functions know which model to use (For log is already added in log function)
  log_lasso_reg <- list(log_lasso_reg, marker, poly)} else {
    
    #In case of zzro degree polynomial (only constant regressors), a normal log model which should
    #predict the mean value of T is calculated instead of the lasso
    PS_pred_lasso <- rep(mean(as.numeric(data$T))-1, nrow(data))
    #Note that the marker of the log model is already added in its own function
    log_lasso_reg <- Log_PS_pred(data = data, polynomials_vector = c(0), return_Model = TRUE)
  }
  
  #If required in the function call, return predicted PS scores AND the model for later usage
  if(return_modelAndPS){PS_pred_lasso <- list(PS_pred_lasso, log_lasso_reg)}
  
  return(PS_pred_lasso)
}

Log_PS_pred <- function(data,
                        polynomials_vector = NA,
                        target_var = "T",
                        vars_to_exclude = c("Y", "PS"),
                        return_Model = FALSE) {
  #Determine vars to drop
  drop_vars <- c(target_var, vars_to_exclude)
  regressors <- names(data)[!names(data) %in% drop_vars]
  if(!is.na(polynomials_vector)){
    regressors <- regressors[regressors!="OneVector"]
    regressors <- c(paste0("I(",regressors,"^",polynomials_vector,")"),"OneVector")
  }
  f <- paste(target_var, "~", paste(regressors, collapse = "+"))
  log_reg <- glm(f, data = data, family = binomial(link = "logit"))
  PS_pred_log <- as.vector(predict.glm(object = log_reg, newdata = data, type = "response"))
  if (return_Model){PS_pred_log <- list(PS_pred_log, list(log_reg, "marker" = "log"))}
  return(PS_pred_log)
}

Probit_PS_pred <- function(data, 
                           target_var = "T",
                           vars_to_exclude = c("Y", "PS")) {
  drop_vars <- c(target_var, vars_to_exclude)
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- paste(target_var, "~", paste(regressors, collapse = "+"))
  probit_reg <- glm(f, data = data, family = binomial(link = "probit"))
  PS_pred_probit <- as.vector(predict.glm(object = probit_reg, newdata = data, type = "response"))
  return(PS_pred_probit)
}

True_PS_pred <- function(data){
  PS_true <- data$PS
  return(PS_true)
}


###
# Inverse probability weighting
###

IPW_fun <- function(PS_est, dataset){
  Y = dataset$Y
  T = as.numeric(dataset$T) - 1
  divisor <- 1/length(T)
  first_term <- (Y*T) / PS_est
  second_term <- (Y*(1-T)) / (1-PS_est)
  TE <- divisor * sum(first_term - second_term)
  return(TE)
}

NIPW_fun <- function(PS_est, dataset){
  Y = dataset$Y
  T = as.numeric(dataset$T) -1
  first_term <- sum((Y*T)/PS_est) / sum(T/PS_est)
  second_term <- sum((Y*(1-T))/(1-PS_est)) / sum((1-T)/(1-PS_est))
  TE <- first_term - second_term
  return(TE)
}

Kernel_fun <- function(dataset, PS_est, bandwidth){
  #Subset data
  treat_indicator <- dataset$T==1
  treat <- dataset[treat_indicator,]
  Ntreat <- dataset[!treat_indicator,]
  Ntreat_PS <- PS_est[!treat_indicator]
  
  #Get weight, given PS and kernel
  get_weight <- function(PS, kernel_dist) {
    pointer <- which.min(abs(kernel_dist$x - PS))
    weight <- kernel_dist$y[pointer]
    return(weight)
  }
  
  # Get weighted outcomes given an indicator of observations that lie within min/max of kernel
  # and the actual kernel
  get_weighted_outcome <- function(indicator, kernel_dist){
    weights <- unlist(lapply(Ntreat_PS[indicator], 
                             get_weight, 
                             kernel_dist = kernel_dist))
    weighted_outcome <- sum(weights * Ntreat[indicator,]$Y)/sum(weights)
    #Is this correct, to just divide by the sum of weights?
    return(weighted_outcome)
  }
  
  #Get list of kernels for all estimated PS values of treated obsersvations
  kernels <- lapply(PS_est[treat_indicator], density, bw = bandwidth, kernel = "gaussian")
  
  #Get list of indicators of whether non_treated PS score lies within boundaries of kernel
  kernel_indicators <- lapply(kernels, function(x) (Ntreat_PS > min(x$x) &
                                                      Ntreat_PS < max(x$x)))
  
  #Get weighted outcomes for each treated observations and its associated indicator and kernel
  weighted_outcomes <-  mapply(get_weighted_outcome, 
                               kernel_indicators, 
                               kernels, 
                               SIMPLIFY = TRUE)
  
  #Take mean of differences between treatment outcomes and and weighted outcomes for each observation
  diffs <- treat$Y - weighted_outcomes
  diffs <- diffs[!is.na(diffs)]
  TE <- mean(diffs)
  return(TE)
  #Probably could be simplified by including kernel_indicators function in in 
  #"get_weighted_outcome" function
}
