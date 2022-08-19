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
                          penalty = "CV",
                          return_modelAndPS = FALSE,
                          return_penalty = FALSE) {
  
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
    if(penalty=="CV"){
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = "binomial", nfolds = nfolds)
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
    } else if (is.numeric(penalty)){
      log_lasso_reg <- glmnet(x=X, y=T, family = "binomial", alpha = 1, lambda = penalty)
    }
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
  if(return_penalty){PS_pred_lasso <- list(PS_pred_lasso, cv.lasso$lambda.min)}
  
  return(PS_pred_lasso)
}

Log_PS_pred <- function(data,
                        polynomials_vector = NA,
                        interaction_terms = NA,
                        target_var = "T",
                        vars_to_exclude = c("Y", "PS"),
                        return_Model = FALSE) {
  #Determine vars to drop
  drop_vars <- c(target_var, vars_to_exclude)
  regressors_raw <- names(data)[!names(data) %in% drop_vars]
  regressors <- regressors_raw
  if(!is.na(polynomials_vector)){
    regressors <- regressors_raw[regressors_raw!="OneVector"]
    regressors <- c(paste0("I(",rep(regressors, each = length(polynomials_vector)),"^",polynomials_vector,")"),"OneVector")
  }
  if(!is.na(interaction_terms)) {
    interactions <- regressors_raw[regressors_raw!="OneVector"]
    #Drop interactions of same variables: 
    polyToInteract <- paste0("I(",rep(interactions, each = length(interaction_terms)),"^",interaction_terms,")")
    interactions <- combn(polyToInteract, m = 2)
    interactions <- paste0(interactions[1,],":",interactions[2,])
    #Drop all interactions between diferent polynomials of the same variable
    indicator <- unlist(lapply(interactions, function (x) 
      substr(strsplit(x, split = ":")[[1]][[1]], 1,4) == substr(strsplit(x, split = ":")[[1]][[2]], 1,4) ))
    interactions <- interactions[!indicator]
    regressors <- c(regressors, interactions)
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
# Double/debiased ML
###

DoubleML_treatment_est <- function(data, 
                                   model = "interactive",
                                   mtry = "half_N") {
  #Prepare data
  dataset <- data[,names(data) != "PS"]
  dataset["T"] = as.numeric(dataset$T)-1
  
  if (mtry =="root_N") {
    n_vars = round((ncol(dataset) - 2)^0.5)
  } else if (mtry == "half_N") {
    n_vars = round((ncol(dataset) - 2)/2)
  } else {n_vars = mtry}
  
  obj_dml_data = double_ml_data_from_data_frame(dataset, y_col = "Y", d_cols = "T")
  
  #Prepare estimator
  lgr::get_logger("mlr3")$set_threshold("warn")
  ml_g = lrn("regr.ranger", num.trees = 100, mtry = n_vars, min.node.size = 2, max.depth = 5)
  ml_m = lrn("classif.ranger", num.trees = 100, mtry = n_vars, min.node.size = 2, max.depth = 5)
  
  if (model == "interactive"){
    obj_dml_plr = DoubleMLIRM$new(obj_dml_data,
                                  ml_g, ml_m,
                                  n_rep = 1,
                                  score = "ATE")
  } else if (model == "partiallyLinear") {
    obj_dml_plr = DoubleMLPLR$new(obj_dml_data,
                                  ml_g, ml_m,
                                  n_folds=5)
  }
  obj_dml_plr$fit()
  estimate = obj_dml_plr$coef
  return(estimate)
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

#Enforcing overlap by trimming all observations with
# estimated PS smaller than smallest estimated PS of treatment group and
# estimated PS larger than biggest estimated PS of non-treated group
enforce_overlap <- function(dataset, PS_est){
  treated <- dataset$T == 1
  Ntreated <- dataset$T == 0
  min_PS_T <- min(PS_est[treated])
  max_PS_NT <- max(PS_est[Ntreated])
  indicator <- PS_est>min_PS_T & PS_est < max_PS_NT
  return(indicator)
}


#Function for sample splitting for cross-fitting
split_sample <- function(dataset, n_folds = 2){
  fold_size <- round(nrow(dataset)/n_folds)
  samples <- list()

  for(i in 1:n_folds){
    test_index <- (1+((i-1)*fold_size)):(i*fold_size)
    test <- dataset[test_index,]
    train <- dataset[-test_index,]
    samples[[paste0("split", i)]]$train <- train
    samples[[paste0("split", i)]]$test <- test
  }
  return(samples)
}

#Function to estimate cross-fitted treatment effect estimates
#First implement with lasso & (N)IPW as default with fixed parameters, can make it more flexible later
cross_fitter <- function(splitted_data, 
                         trimming = TRUE){
  Lasso_out <- Lasso_PS_pred(data = splitted_data$train,
                             return_modelAndPS = TRUE)
  model <- Lasso_out[[2]][[1]]
  marker <- Lasso_out[[2]][[2]]
  
  data_pred <- as.matrix(splitted_data$test[,!names(splitted_data$test) %in% c("Y", "T", "PS")])
  
  PS_pred <- as.vector(predict(object = model, 
                               newx = data_pred,
                               type = "response"))
  if (trimming == TRUE){
    quantiles <- stats::quantile(PS_pred, c(0.02,0.98))
    indicator <- (splitted_data$test$PS > quantiles[[1]] & splitted_data$test$PS < quantiles[[2]])
  }

  if(mean(indicator>0.95)){
    PS_pred <- PS_pred[indicator]
    splitted_data$test <- splitted_data$test[indicator,]
  }

  IPW_results <- IPW_fun(PS_est = PS_pred,
                              dataset =  splitted_data$test)
  NIPW_results <- NIPW_fun(PS_est = PS_pred, 
                                dataset = splitted_data$test)
  return(list("IPW" = IPW_results, "NIPW" = NIPW_results))
}

estimate_aggregator <- function(fitted_values_list){
  IPW_results <- lapply(fitted_values_list, function(x) return(x[[1]]))
  NIPW_results <- lapply(fitted_values_list, function(x) return(x[[2]]))
  return(list("IPW" = mean(unlist(IPW_results)), 
         "NIPW" = mean(unlist(NIPW_results))))
}


