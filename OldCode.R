#Probit implementation
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


####
# Old functions from generate data
####

gen_X <- function(n = 1, n_obs = 500, covar = 0, min = -1, max = 1,
                  dummy_share = 0, X_dummy_shift_percent = 0, prob_dummy = 0.5,
                  factor_share = 0) {
  #Create covariates, (by default drawn from uniform distribution)
  # n = number of covariates
  # n_obs = number of rows in the data
  # min_X / max_X = minimum / maximum of uniform distribution values are drawn from
  # covar = correlation between pairs of covariates (apply only on even number of covariates)
  # dummy_share = share of covariates in dataset that are drawn from binomial dist 
  # prob_dummy = probability of drawing a 1
  # factor_share = share of factor variables (values drawn from [-1, -0.3, 0.3,1]) in dataset
  # prob_dummy = probability of 1 for dummies
  
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



# Initial replication of Millimet & Tchernis paper
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

###
#Inital outcome creation function but already adapted to Millimet & Tchernis paper
###
gen_Y_old <- function(X, T, X_impact_share = 1, X_impact_shift_percent = 0, error = FALSE, 
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

###
# Old PS functions from many older experiments
###
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

gen_PS_highDim <- function(X, impact_share, beta = 6, beta_adjust_power = 1, alpha = -3, alpha_adjust_power = 1){
  N_treat <- ceiling(ncol(X) * impact_share)
  beta <- beta/(N_treat^beta_adjust_power)
  alpha <- alpha/(N_treat^alpha_adjust_power)
  X_impact <- matrix(X[,1:N_treat], ncol = N_treat)
  PS_raw <- alpha * N_treat + beta * rowSums(X_impact)
  PS <- exp(PS_raw)/(exp(PS_raw)+1)
  return(PS)
}


###
# Old functions from ModelImplementations
###
###
# Neural network
###

NN_PS_pred <- function(data,
                       target_var = "T",
                       vars_to_exclude = c("Y", "PS"),
                       epochs = 100,
                       #epochs = c(50, 100),
                       dropout = c(0.001, 0.01, 0.05, 0.1, 0.2),
                       penalty = c(0.00001, 0.0001, 0.001,0.01,0.1,0.2),
                       hidden_units = c(1, 3, 5, 7, 10, 20)) {
  
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications 
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- as.formula(paste(target_var, "~", paste(regressors, collapse = "+")))
  
  nn_grid <- expand.grid(#epochs = epochs,
    #dropout = dropout,
    penalty = penalty,
    hidden_units = hidden_units
  )
  #Creating cross-validation of dataset
  data_cv <- vfold_cv(data, v = 5)
  
  nn <- mlp(#epochs = tune(), 
    epochs = epochs,
    penalty = tune(),
    #dropout = tune(),
    hidden_units = tune()) %>%
    set_mode("classification") %>%
    set_engine("keras", verbose  = 0)
  
  nn_recipe <- recipe(f, data = data) %>%
    #step_BoxCox(all_predictors())%>%
    step_normalize(all_predictors()) %>%
    prep(training = data, retain = TRUE)
  
  nn_workflow <- workflow() %>% add_recipe(nn_recipe) %>% add_model(nn)
  
  nn_tune_results <- nn_workflow %>% tune_grid(resamples = data_cv,
                                               grid = nn_grid,
                                               metrics = metric_set(accuracy, roc_auc))
  #Extract best parameters
  param_final <- nn_tune_results %>% 
    select_best(metric = "accuracy")
  
  #Use best parameters
  nn_workflow <- nn_workflow %>% finalize_workflow(param_final)
  
  #Fit final model
  final_model <- fit(nn_workflow, data)
  
  #Predict (on existing data)
  NN_PS <- as.vector(predict(final_model, new_data = data, type = "prob")$.pred_1)
  #NN_PS <- as.vector(final_model$fit$fit$fit$predict()) #Is not working, approach above should be fine
  return(NN_PS)
  
  # nnet_fit <-
  #   mlp(epochs = epochs, hidden_units = hidden_units, dropout = dropout) %>%
  #   set_mode("classification") %>% 
  #   # Also set engine-specific `verbose` argument to prevent logging the results: 
  #   set_engine("keras", verbose = 0) %>%
  #   fit(f, data = bake(biv_rec, new_data = NULL))
  # 
  # NN_PS <- predict(nnet_fit, new_data = dataset, type = "prob")$.pred_1
  # return(NN_PS)
}

###
# Implementation of RF
###
#Needs some more thinking about parameter grid

RF_PS_pred <- function(data, 
                       target_var = "T", 
                       vars_to_exclude = c("Y", "PS"),
                       get_var_importance = FALSE,
                       n_trees = 75,
                       #n_trees_grid = c(20,40,100, 200),
                       mtry_grid = c(1,2,3,6, 10, 20, 50, 100, 200, 500, 750),
                       min_n_grid = c(1, 2, 5, 10,20,30,50)
) {
  rf <- rand_forest(mode = "classification",
                    mtry = tune(),
                    trees = n_trees,
                    #trees = tune(),
                    min_n = tune()) %>%
    set_engine("ranger", importance = "impurity")
  
  #Make grid smaller if it is not necessary
  mtry_grid <- mtry_grid[mtry_grid < ncol(data)]
  
  #Specify parameter grid
  rf_grid <- expand.grid(mtry = mtry_grid,
                         #trees = n_trees_grid,
                         min_n = min_n_grid)
  
  #Creating cross-validation of dataset
  data_cv <- vfold_cv(data, v = 5)
  
  #Define recipe (which includes pre-processing and the regression formula)
  if (all(is.na(vars_to_exclude))) {cols = colnames(data)} else {cols = colnames(data[!names(data) %in% vars_to_exclude])}
  
  recipe <- recipe(data[,cols]) %>%
    update_role(target_var, new_role = "outcome") %>%
    update_role(-target_var, new_role = "predictor") %>%
    step_mutate(target_var = as.factor(target_var))
  
  #Combine in a workflow
  rf_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf)
  
  #Tune Hyperparameters
  #If it fails, reason might be that dependent var is not factor
  rf_tune_results <- rf_workflow %>% tune_grid(resamples = data_cv,
                                               grid = rf_grid)#, 
  #            metrics = metric_set(accuracy, roc_auc))
  
  #Extract best parameters
  param_final <- rf_tune_results %>% 
    select_best(metric = "accuracy")
  
  #Use best parameters
  rf_workflow <- rf_workflow %>% finalize_workflow(param_final)
  
  #Fit final model
  final_model <- fit(rf_workflow, data)
  
  #Get variable importance
  #pull_workflow_fit(final_model)$fit$variable.importance
  
  #Predict (on existing data)
  #RF_PS <- as.vector(predict(final_model, new_data = data, type = "prob")$.pred_1)
  RF_PS <- as.vector(final_model$fit$fit$fit$predictions[,2])
  return(RF_PS)
}

###
# Implement alternative random forest since other one seems to not be working well
###

#For testing purposes, also create validation set
RF_PS_pred_2 <- function(data,
                         target_var = "T",
                         vars_to_exclude = c("Y", "PS")){
  
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  formula <- paste(target_var, "~", paste(regressors, collapse = "+"))
  f <- as.formula(formula)
  rf <- randomForest(f, data = data)
  #RF_PS <- as.vector(predict(rf, Trainset, type = "prob")[,2]
  RF_PS2 <- as.vector(predict(rf, type = "prob")[,2])
  return(RF_PS2)
}

###
# Bayesian Regression (ridge, lasso, horseshoe, horseshoe+)
###
#https://cran.r-project.org/web/packages/bayesreg/bayesreg.pdf

Bayes_Ridge_PS_pred <- function(data,
                                include_polynomials = TRUE,
                                include_interactions = TRUE,
                                target_var = "T",
                                vars_to_exclude = c("Y", "PS")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  formula <- paste(target_var, "~", paste(regressors, collapse = "+"))
  if (include_interactions & length(regressors) > 1) {
    interactions <- paste(combn(regressors, 2, FUN=paste, collapse=':'), collapse = "+")
    formula <- c(formula,"+", interactions)}
  if (include_polynomials) {
    polynomials <- paste0(regressors, "^2", collapse = "+")
    formula <- c(formula, "+", polynomials)}
  f <- as.formula(paste(formula, collapse = ""))
  bayes_ridge_reg <- bayesreg(f, data = data, model = "logistic", prior = "ridge")
  PS_pred_bayes_ridge <- as.vector(predict(object = bayes_ridge_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_ridge)
}

Bayes_Lasso_PS_pred <- function(data, 
                                include_polynomials = TRUE,
                                include_interactions = TRUE,
                                target_var = "T",
                                vars_to_exclude = c("Y", "PS")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  formula <- paste(target_var, "~", paste(regressors, collapse = "+"))
  
  if (include_interactions & length(regressors) > 1) {
    interactions <- paste(combn(regressors, 2, FUN=paste, collapse=':'), collapse = "+")
    formula <- c(formula,"+", interactions)}
  if (include_polynomials) {
    polynomials <- paste0(regressors, "^2", collapse = "+")
    formula <- c(formula, "+", polynomials)}
  f <- as.formula(paste(formula, collapse = ""))
  bayes_lasso_reg <- bayesreg(f, data = data, model = "logistic", prior = "lasso")
  PS_pred_bayes_lasso <- as.vector(predict(object = bayes_lasso_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_lasso)
}

Bayes_Horseshoe_PS_pred <- function(data,  
                                    include_polynomials = TRUE,
                                    include_interactions = TRUE,
                                    target_var = "T",
                                    vars_to_exclude = c("Y", "PS")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  formula <- paste(target_var, "~", paste(regressors, collapse = "+"))
  
  if (include_interactions & length(regressors) > 1) {
    interactions <- paste(combn(regressors, 2, FUN=paste, collapse=':'), collapse = "+")
    formula <- c(formula,"+", interactions)}
  if (include_polynomials) {
    polynomials <- paste0(regressors, "^2", collapse = "+")
    formula <- c(formula, "+", polynomials)}
  f <- as.formula(paste(formula, collapse = ""))
  bayes_horseshoe_reg <- bayesreg(f, data = data, model = "logistic", prior = "horseshoe")
  PS_pred_bayes_horseshoe <- as.vector(predict(object = bayes_horseshoe_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_horseshoe)
}

Bayes_HorseshoePlus_PS_pred <- function(data,  
                                        include_polynomials = TRUE,
                                        include_interactions = TRUE,
                                        target_var = "T",
                                        vars_to_exclude = c("Y", "PS")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  formula <- paste(target_var, "~", paste(regressors, collapse = "+"))
  
  if (include_interactions & length(regressors) > 1) {
    interactions <- paste(combn(regressors, 2, FUN=paste, collapse=':'), collapse = "+")
    formula <- c(formula,"+", interactions)}
  if (include_polynomials) {
    polynomials <- paste0(regressors, "^2", collapse = "+")
    formula <- c(formula, "+", polynomials)}
  f <- as.formula(paste(formula, collapse = ""))
  bayes_horseshoePlus_reg <- bayesreg(f, data = data, model = "logistic", prior = "horseshoe+")
  PS_pred_bayes_horseshoePlus <- as.vector(predict(object = bayes_horseshoePlus_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_horseshoePlus)
}


LassoNaive_PS_pred <- function(data, 
                               target_var = "T", 
                               vars_to_exclude = c("Y", "PS"), 
                               family = "binomial") {
  drop_vars <- c(target_var, vars_to_exclude)
  X <- as.matrix(data[,!names(data) %in% drop_vars])
  T <- as.matrix(data[,target_var])
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = family, nfolds = 10)
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
  PS_pred_lasso <- as.vector(predict(object = log_lasso_reg, newx = X, type = "response"))
  return(PS_pred_lasso)
}

Ridge_PS_pred <- function(data, 
                          target_var = "T",
                          include_interactions = TRUE,
                          include_polynomials = TRUE,
                          vars_to_exclude = c("Y", "PS"), 
                          family = "binomial",
                          return_modelAndPS = FALSE) {
  drop_vars <- c(target_var, vars_to_exclude)
  raw_X <- as.matrix(data[,!names(data) %in% drop_vars])
  X = raw_X
  if (include_interactions & (ncol(raw_X) > 2)) {
    interactions <- model.matrix( ~.^2, data=data.frame(raw_X[,-ncol(raw_X)]))[,-c(1:ncol(raw_X))]
    X <- cbind(X, interactions)}
  if (include_polynomials) {
    polynomials <- raw_X^2 
    colnames(polynomials) <- paste0(colnames(polynomials), "_squared")
    X <- cbind(X, polynomials[,-ncol(raw_X)])}#Drop last column bc. of "oneVector"
  X <- as.matrix(X)
  
  T <- as.matrix(data[,target_var])
  cv.ridge <- cv.glmnet(x = X, y = T, alpha = 0, family = family, nfolds = 5)
  log_ridge_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 0, lambda = cv.ridge$lambda.min) 
  PS_pred_ridge <- as.vector(predict(object = log_ridge_reg, newx = X, type = "response"))
  if(return_modelAndPS){PS_pred_ridge <- list(PS_pred_ridge, log_ridge_reg)}
  return(PS_pred_ridge)
}


Constant_PS_pred <- function(data){
  mean_T <- mean(as.numeric(data$T))-1
  PS_mean <- as.vector(rep(mean_T, nrow(data)))
  return(PS_mean)
}

ZeroOFive_PS_pred <- function(data){
  ZeroOFive_PS <- as.vector(rep(0.5,nrow(data)))
  return(ZeroOFive_PS)
}



###
# Double/debiased ML
###
#Not directly used as PS estimator, but directly estimates treatment effect; hence
#later called in the "estimate_TE" function

DoubleML_treatment_est <- function(data) {
  #Prepare data
  dataset <- data[,names(data) != "PS"]
  n_vars = ncol(dataset) - 2
  dataset["T"] = as.numeric(dataset$T)
  obj_dml_data = double_ml_data_from_data_frame(dataset, y_col = "Y", d_cols = "T")
  
  #Prepare estimator
  lgr::get_logger("mlr3")$set_threshold("warn")
  learner = lrn("regr.ranger", num.trees=100, mtry=n_vars, min.node.size=2, max.depth=5)
  ml_m = learner$clone()
  ml_g = learner$clone()
  obj_dml_plr = DoubleMLPLR$new(obj_dml_data,
                                ml_g, ml_m,
                                n_folds=2,
                                score='IV-type')
  obj_dml_plr$fit()
  estimate = obj_dml_plr$coef
  return(estimate)
}

DoubleML_treatment_est <- function(data) {
  #Prepare data
  dataset <- data[,names(data) != "PS"]
  dataset["T"] = as.numeric(dataset$T)
  obj_dml_data = double_ml_data_from_data_frame(dataset, y_col = "Y", d_cols = "T")
  
  #Prepare estimator
  lgr::get_logger("mlr3")$set_threshold("warn")
  learner = lrn("regr.ranger", num.trees=100, mtry=n_vars, min.node.size=2, max.depth=5)
  ml_m = learner$clone()
  ml_g = learner$clone()
  obj_dml_plr = DoubleMLPLR$new(obj_dml_data,
                                ml_g, ml_m,
                                n_folds=2)
  obj_dml_plr$fit()
  estimate = obj_dml_plr$coef
  return(estimate)
}

DoubleML_treatment_est <- function(data, 
                                   model = "interactive",
                                   mtry = "half_N") {
  #Prepare data
  dataset <- data[,names(data) != "PS"]
  dataset["T"] = as.numeric(dataset$T)-1
  
  if (mtry =="roor_N") {
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
# Combine PS estimators 
###
#Main function that can call all different PS estimators and returns list of predicted PS scores

PS_estimators <- function(data,
                          target_var = "T",
                          vars_to_exclude = c("Y", "PS"),
                          estimators = c("true", "log", "probit", "meanT", "zeroOFive", "ridge", "lasso", "rf",
                                         "bayes_ridge", "bayes_lasso", "bayes_horseshoe", "bayes_horseshoePlus", "nn")) {
  #Function takes data, uses all matching estimators specified and returns list with estimated propensity scores
  results <- list() 
  if ("true") {
    results <- append(results, list("true" = 
                                      True_PS_pred(data)))}
  if ("log" %in% estimators) {
    results <- append(results, list("log" = 
                                      Log_PS_pred(data, target_var, vars_to_exclude)))}
  if ("probit" %in% estimators) {
    results <- append(results, list("probit" = 
                                      Probit_PS_pred(data, target_var, vars_to_exclude)))}
  if ("meanT" %in% estimators) {
    results <- append(results, list("meanT" = 
                                      Constant_PS_pred(data)))}
  if ("zeroOFive" %in% estimators) {
    results <- append(results, list("zeroOFive" = 
                                      ZeroOFive_PS_pred(data)))}
  if ("ridgeNaive" %in% estimators) {
    results <- append(results, list("ridgeNaive" = 
                                      Ridge_PS_pred(data, target_var, vars_to_exclude, include_interactions = FALSE, include_polynomials = FALSE)))}
  if ("ridgeInterPoly" %in% estimators) {
    results <- append(results, list("ridgeFull" = 
                                      Ridge_PS_pred(data, target_var, vars_to_exclude, include_interactions = TRUE, include_polynomials = TRUE)))}
  if ("lassoNaive" %in% estimators) {
    results <- append(results, list("lassoNaive" = 
                                      Lasso_PS_pred(data, target_var, vars_to_exclude, include_interactions = FALSE, include_polynomials = FALSE)))}
  if ("lassoInterPoly" %in% estimators) {
    results <- append(results, list("lassoFull" = 
                                      Lasso_PS_pred(data, target_var, vars_to_exclude, include_interactions = TRUE, include_polynomials = TRUE)))}
  if ("rf" %in% estimators) {
    results <- append(results, list("rf" = 
                                      suppressMessages(RF_PS_pred(data, target_var, vars_to_exclude))))} 
  if ("rf2" %in% estimators) {
    results <- append(results, list("rf2" = 
                                      suppressMessages(RF_PS_pred_2(data, target_var, vars_to_exclude))))} 
  if ("bayes_ridgeInterPoly" %in% estimators) {
    results <- append(results, list("bayes_ridgeFull" = 
                                      Bayes_Ridge_PS_pred(data, target_var, vars_to_exclude, include_interactions = TRUE, include_polynomials = TRUE)))}
  if ("bayes_lassoInterPoly" %in% estimators) {
    results <- append(results, list("bayes_lassoFull" = 
                                      Bayes_Lasso_PS_pred(data, target_var, vars_to_exclude, include_interactions = TRUE, include_polynomials = TRUE)))}
  if ("bayes_horseshoeInterPoly" %in% estimators) {
    results <- append(results, list("bayes_horseshoeFull" = 
                                      Bayes_Horseshoe_PS_pred(data, target_var, vars_to_exclude, include_interactions = TRUE, include_polynomials = TRUE)))}
  if ("bayes_horseshoePlusInterPoly" %in% estimators) {
    results <- append(results, list("bayes_horseshoePlusFull" = 
                                      Bayes_HorseshoePlus_PS_pred(data, target_var, vars_to_exclude, include_interactions = TRUE, include_polynomials = TRUE)))}
  if ("nn" %in% estimators) {
    results <- append(results, list("nn" = 
                                      NN_PS_pred(data, target_var, vars_to_exclude)))}
  return(results)
}

###
# Estimate TE
###
#This should estimate treatment effects given all combinations of matches
estimate_TE <- function(list_of_matches = NA, data, PS_scores,
                        include_DML = TRUE, 
                        include_SimpleReg = TRUE,
                        include_InterPolyReg = TRUE,
                        include_IPW = TRUE,
                        include_NIPW = TRUE){
  keys = c()
  reg_results = c()
  if(!is.na(list_of_matches)){
    for (i in seq_along(list_of_matches)) {
      matching_estimator = names(list_of_matches)[i]
      for (y in seq_along(list_of_matches[[i]])) {
        ps_estimator = names(list_of_matches[[i]])[y]
        #key = noquote(paste0("Matching Estimator: ", matching_estimator, ", PS_Estimator: ", ps_estimator))
        key = paste0(matching_estimator, "_", ps_estimator)
        keys = c(keys, key)
        reg = lm(Y~T, data = list_of_matches[[i]][[y]]$matched_data, weights = list_of_matches[[i]][[y]]$weights) #Somehow not working if Y~T is passed as formula object
        reg_results <- c(reg_results, reg$coefficients[["T1"]])
      }
    }
  }
  results <- setNames(as.list(reg_results), keys)
  #Add double ML result
  if(include_DML){
    double_ML_estimate <- DoubleML_treatment_est(data = data)[[1]]
    results$DML <- double_ML_estimate}
  if(include_SimpleReg) {
    reg_estimate_noCovar <- lm(Y~T, data = data)
    results$RegNoCovar <- coef(reg_estimate_noCovar)[["T1"]]
    
    regressors <- names(data)[!names(data) %in% c("PS", "Y")]
    f <- paste("Y", "~", paste(regressors, collapse = "+"))
    reg_estimate_Covar <- lm(f, data = data)
    results$RegCovar <- coef(reg_estimate_Covar)[["T1"]]
  }
  if(include_InterPolyReg){
    regressors <- names(data)[!names(data) %in% c("PS", "Y")]
    formula <- paste("Y", "~", paste(regressors, collapse = "+"))
    if (length(regressors) > 1) {
      interactions <- paste(combn(regressors, 2, FUN=paste, collapse=':'), collapse = "+")
      formula <- c(formula,"+", interactions)}
    polynomials <- paste0(regressors, "^2", collapse = "+")
    formula <- c(formula, "+", polynomials)
    f <- as.formula(paste(formula, collapse = ""))
    reg_estimate_InterPoly <- lm(f, data = data)
    results$RegInterPoly <- coef(reg_estimate_InterPoly)[["T1"]]
  }
  if(include_IPW){
    IPW <- lapply(
      PS_scores, IPW_fun, dataset = data)
    names(IPW) <- paste0("IPW_", names(IPW))
    results <- append(results, IPW)
  }
  if(include_NIPW){
    NIPW <- lapply(
      PS_scores, NIPW_fun, dataset = data) 
    names(NIPW) <- paste0("NIPW_", names(NIPW))
    results <- append(results, NIPW)
  }
  return(results)
}

est_var_w_polynomials <- function(dataset, PS_score, TE, poly_degree = 1){
  T <- as.numeric(dataset$T)-1
  Y <- dataset$Y
  X <- dataset$X
  if (poly_degree == 0) {
    X_matrix = rep(0, length(Y))} else{
      X_matrix <- poly(X, degree = poly_degree, raw = TRUE)
    }
  N <- nrow(dataset)
  pheta_vec <- ((Y*T)/PS_score) - ((Y*(1-T))/(1-PS_score)) - TE
  alpha_part_one <- t((1/N)*sum((((Y*T)/PS_score^2) + ((Y*(1-T))/((1-PS_score)^2)))*X))
  alpha_part_two <- (1/N)*sum(X%*%t(X)) 
  alpha_part_three <- X*(T-PS_score)
  alpha_vec <- -1*(as.numeric(alpha_part_one/alpha_part_two)) * alpha_part_three
  V_est <- (1/N)*sum((pheta_vec+alpha_vec)^2)
  return(V_est)
}



###
# Simulations
###
#sfExport("paste0")

# sim_VM <- function(n_obs, DGPS) {
#   if (DGPS == "simple"){
#     dataset = genDS_MV_simple(n_obs = n_obs)}
#   else if (DGPS == "PS_error"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               v_range = c(-1,1))}
#   else if (DGPS == "Y_error"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               e_range = c(-1,1))}
#   else if (DGPS == "400var"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               n_X = 400)}
#   else if (DGPS == "250var"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               n_X = 250)}
#   else if (DGPS == "x_on_y"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               alpha_outcome = 2)}
#   else if (DGPS == "multi_col"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               X_dist = "normal_cor")}
#   else if (DGPS == "nonLinear_1"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               PS_link = "nonLinear_1")}
#   else if (DGPS == "het_TE"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               Y_link = "het_TE")}
#   else if (DGPS == "many_irrelevant_X"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               PS_estimation = "many_irrelevant_X",
#                               n_X = 200)}
#   else if (DGPS == "lechner"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               treatment_assignment = "lechner")}
#   else if (DGPS == "two_norm_dist"){
#     dataset = genDS_MV_simple(n_obs = n_obs,
#                               X_dist = "two_norm_dist")}
#   
#   PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
#   TE_effects <- estimate_TE(matched_data)
#   bias <- lapply(TE_effects, function(x) x - 10)
#   return(bias)
# }
# 
# param_list = list("n_obs" = c(200, 1000), 
#                   "DGPS" = c("lechner", "two_norm_dist"))
# 
# sim_VM <- function(var, alpha_PS, alpha_outcome){
#   
#   dataset <- genDS_MV_varying_cov(n_obs = 300, var = var, alpha_PS=alpha_PS, 
#                                   alpha_outcome=alpha_outcome)
#   
#   PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
#   TE_effects <- estimate_TE(matched_data)
#   bias <- lapply(TE_effects, function(x) x - 10)
#   return(bias)
# }
# 
# param_list = list("var" = c(0,0.3,0.6,0.9,0.99),
#                   "alpha_PS" = c(0.1,0.5,1,3,10), #Could have different parameters for the different X
#                   "alpha_outcome" = c(0,0.5,1))



# sim_VM <- function(n_obs,
#                    alpha_PS,
#                    Y_error,
#                    DGP){
#   if(DGP=="simple"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error)
#   }
#   if(DGP=="x_y_not_unif"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        x_y_not_unif = TRUE)
#   }
#   if(DGP=="multi_col_01"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        colinearity = TRUE, covar = 0.1)
#   }
#   if(DGP=="multi_col_04"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        colinearity = TRUE, covar = 0.4)
#   }
#   if(DGP=="multi_col_07"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        colinearity = TRUE, covar = 0.7)
#   }
#   if(DGP=="multi_col_095"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        colinearity = TRUE, covar = 0.95)
#   }
#   if(DGP=="ps_error"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        PS_error = TRUE)
#   }
#   if(DGP=="n_X_100"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        n_X = 100)
#   }
#   if(DGP=="n_X_1000"){
#     dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#                        n_X = 1000)
#   }
#   # if(DGP=="TA_lechner_1"){
#   #   dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#   #                      TA_lechner = TRUE)
#   # }
#   # if(DGP=="TA_lechner_2"){
#   #   dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#   #                      TA_lechner = TRUE, 
#   #                      alpha_treat = 0.5)
#   # }
#   # if(DGP=="TA_lechner_3"){
#   #   dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#   #                      TA_lechner = TRUE, 
#   #                      lambda_treat = 0.5)
#   # }
#   # if(DGP=="TA_lechner_4"){
#   #   dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#   #                      TA_lechner = TRUE, 
#   #                      alpha_treat = 0.5,
#   #                      lambda_treat = 0.5)
#   # }
#   # if(DGP=="TA_lechner_5"){
#   #   dataset <- genDS_1(n_obs = n_obs, alpha_PS = alpha_PS, Y_error = Y_error,
#   #                      TA_lechner = TRUE, 
#   #                      lambda_treat = 1)
#   # }
#   
#   PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
#   TE_effects <- estimate_TE(matched_data)
#   bias <- lapply(TE_effects, function(x) x - 10)
#   return(bias)
# }
# 
# param_list <- list(n_obs = c(200,1000),
#                    alpha_PS = c(0.1,1,5),
#                    #alpha_PS = c(0.1,0.5,1,3,10),
#                    Y_error = c(0,1),
#                    DGP = c("simple", "x_y_not_unif",
#                            "multi_col_01", "multi_col_095",
#                            #"multi_col_01", "multi_col_04", "multi_col_07", "multi_col_095",
#                            # "TA_lechner_1", "TA_lechner_2", "TA_lechner_3","TA_lechner_4","TA_lechner_5",
#                            "n_X_100", "n_X_1000", "ps_error"))

# sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 15, param_list = param_list, time_n_test = TRUE, ncpus = 16)
# save(sim_VM_result, file = "sim_VM_5.Rdata")

# 
# sim_VM <- function(#n_obs,
#   #n_X,
#   #treatment_effect,
#   PS_link,
#   Y_link,
#   rescaleLim,
#   add_X) {
#   if (add_X == "none") {
#   dataset <- genDS_high_dim(n_obs = 1400,
#                             #n_X = n_X,
#                             #treatment_effect = treatment_effect,
#                             PS_link = PS_link,
#                             Y_link = Y_link,
#                             rescaleLim = rescaleLim)}
#   if (add_X == "high_cov") {
#     dataset <- genDS_high_dim(n_obs = 1400,
#                               #n_X = n_X,
#                               #treatment_effect = treatment_effect,
#                               PS_link = PS_link,
#                               Y_link = Y_link,
#                               rescaleLim = rescaleLim,
#                               high_cov = TRUE)}
#   if (add_X == "lin_comb") {
#     dataset <- genDS_high_dim(n_obs = 1400,
#                               #n_X = n_X,
#                               #treatment_effect = treatment_effect,
#                               PS_link = PS_link,
#                               Y_link = Y_link,
#                               rescaleLim = rescaleLim,
#                               lin_comb = TRUE)}
#   if (add_X == "both") {
#     dataset <- genDS_high_dim(n_obs = 1400,
#                               #n_X = n_X,
#                               #treatment_effect = treatment_effect,
#                               PS_link = PS_link,
#                               Y_link = Y_link,
#                               rescaleLim = rescaleLim,
#                               lin_comb = TRUE,
#                               high_cov = TRUE)}
#   PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
#   print(paste0("PS_link: ", PS_link, ";  Y_link: ", Y_link, ";  rescale Limit: ", rescaleLim, 
#               ";  additional variables: ", add_X))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
#   TE_effects <- estimate_TE(matched_data)
#   bias <- lapply(TE_effects, function(x) x - 10)
#   return(bias)
# }
# 
# param_list <- list(#n_obs = c(200, 1000),
#   #n_X = c(5, 100),
#   #treatment_effect = c(0,1,10)
#   PS_link = c("simple", "polynomial", "kink", "random"),
#   Y_link = c("simple", "polynomial", "kink", "random"),
#   rescaleLim = c(2,10),
#   add_X = c("none", "lin_comb", "high_cov", "both"))
# 
#  #!So far, has not been working
# sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 15, param_list = param_list, time_n_test = TRUE, ncpus = 16)
# save(sim_VM_result, file = "sim_VM_6.Rdata")

# sim_VM <- function(#PS_link,
#   #n_obs,
#   alpha_PS,
#   beta_PS,
#   cor_X){
#   dataset <- gen_multiple_easy_DS(n_obs = 1000,
#                                   X_dim = 2,
#                                   PS_link = "probit",
#                                   alpha_outcome = 1,
#                                   alpha_PS = alpha_PS,
#                                   beta_PS = beta_PS,
#                                   cor_X = cor_X)
#   PS_scores <- PS_estimators(dataset,
#                              estimators = c("true", "log", "probit", "ridge", "lasso", "rf",
#                                             "bayes_ridge", "bayes_lasso", "bayes_horseshoe", "bayes_horseshoePlus", "nn"),
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS_scaled"))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk", "stratum", "cs", "caliper_est", "cs"))
#   TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE)
#   bias <- lapply(TE_effects, function(x) x - 1)
#   return(bias)
# }
# 
# param_list <- list(#"n_obs" = c(500, 3000),
#                    "alpha_PS" = c(-1, 0),
#                    "beta_PS" = c(0.5, 1),
#                    "cor_X" = c(0, 0.5, 0.9))
# 
# sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 5,
#                             param_list = param_list, time_n_test = FALSE, ncpus = 16)
# save(sim_VM_result, file = "sim_VM_14.Rdata")

#Looking for bugs
# sim_VM_test <- function(#PS_link,
#   alpha_PS,
#   beta_PS,
#   cor_X){
#   dataset <- gen_multiple_easy_DS(n_obs = 500, 
#                                   X_dim = 2,
#                                   PS_link = "probit", 
#                                   alpha_outcome = 1, 
#                                   alpha_PS = alpha_PS,
#                                   beta_PS = beta_PS,
#                                   cor_X = cor_X)
# 
#   set_up = paste(alpha_PS, beta_PS, cor_X)
#   return = list("PS_scaled_is.na" = sum(is.na(dataset$PS_scaled)),
#                 "T_is.na" = sum(is.na(dataset$T)),
#                 "PS_min" = min(dataset$PS_scaled),
#                 "PS_max" = max(dataset$PS_scaled),
#                 "mean" = mean(dataset$PS_scaled))
#   }
# sim_VM_test_result <- MonteCarlo(func = sim_VM_test, nrep = 30, 
#                             param_list = param_list, time_n_test = FALSE)

# sim_VM <- function(dim_x, alpha){
#   dataset <- DoubleML::make_plr_CCDDHNR2018(return_type = "data.frame",
#                                             dim_x = dim_x, alpha = alpha)
#   colnames(dataset)[colnames(dataset) == 'd'] <- 'PS_scaled'
#   colnames(dataset)[colnames(dataset) == 'y'] <- 'Y'
#   PS_scaled <- rescale(dataset$PS_scaled, to = c(0,1))
#   dataset$PS_scaled <- PS_scaled
#   dataset$T <- rbinom(n=nrow(dataset), 1, prob = PS_scaled)
#   dataset$T <- as.factor(dataset$T)
#   PS_scores <- PS_estimators(dataset, 
#                              estimators = c("true", "log", "probit", "ridge", "lasso", "rf"), 
#                              target_var = "T", 
#                              vars_to_exclude = c("Y", "PS_scaled"))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk")) 
#   TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE)
#   bias <- lapply(TE_effects, function(x) x - 1)
#   return(bias)
# }
# 
# param_list <- list("dim_x" = c(10, 50, 100, 200),
#                    "alpha" = c(0, 0.5, 1, 2))
# 
# sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 25, 
#                             param_list = param_list, time_n_test = FALSE, ncpus = 16)
# save(sim_VM_result, file = "sim_VM_9.Rdata")

# sim_VM <- function(
#   #n_obs,
#   X_dim,
#   #X_impact_share,
#   alpha_PS,
#   beta_PS){
#   dataset <- gen_DS_modular(n_obs = 3000, X_dim = X_dim, X_impact_share = 0.3, 
#                             alpha_PS = alpha_PS, beta_PS = beta_PS)
#   # if (X_range == "large") {
#   #   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, X_impact_share = X_impact_share, alpha_PS = alpha_PS, beta_PS = beta_PS, min_X = -2, max_X = 2)
#   # }
#   # if (X_range == "small") {
#   #   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, X_impact_share = X_impact_share, alpha_PS = alpha_PS, beta_PS = beta_PS, min_X = -0.5, max_X = 0.5)
#   # }
#   
#   PS_scores <- PS_estimators(dataset,
#                              estimators = c("true", "log", "probit", "ridge", "lasso", "rf",
#                                             "bayes_ridge", "bayes_lasso", "bayes_horseshoe", "bayes_horseshoePlus"),
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = c("nn_one", "nnk", "stratum", "cs", "caliper_est"))
#   TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE)
#   bias <- lapply(TE_effects, function(x) x - 1)
#   return(bias)
# }
# 
# param_list <- list(#"n_obs" = c(500, 3000),
#                    "X_dim" = c(1, 5, 10, 18),
#                    #"X_impact_share" = c(0.001, 0.01, 0.05),
#                    "alpha_PS" = c(-0.5, 0),
#                    "beta_PS" = c(0.5, 1))
#                    #"X_range" = c("normal", "small", "large"))
# 
# 
# sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 15,
#                             param_list = param_list, time_n_test = FALSE, ncpus = 16)
# save(sim_VM_result, file = "sim_VM_14.Rdata")


# sim_VM <- function(
#   alpha_PS,
#   PS_link,
#   X_dim,
#   X_impact_share_PS,
#   X_range,
#   cor_X,
#   DGP){
# 
#   Y_impact = "linear"
#   X_impact_share_Y = 1
#   X_impact_shift_percent = 0
#   if (X_range == "normal") {
#     min_X = -1
#     max_X = 1
#   }
#   if (X_range == "large") {
#     min_X = -1.5
#     max_X = 1.5
#   }
#   if (X_range == "small") {
#     min_X = -0.5
#     max_X = 0.5
#   }
# 
#   if (DGP == "quadratic") {Y_impact == "quadratic"}
#   if (DGP == "p = 600") {
#     X_dim = 600
#     X_impact_share = 0.001
#   }
#   if (DGP == "non-linear") {Y_impact == "non_linear"}
#   if (DGP == "Y_impact_shift_1") {
#     X_impact_share_Y = X_impact_share_PS
#   }
#   if (DGP == "Y_impact_shift_2") {
#     X_impact_share_Y = X_impact_share_PS
#     X_impact_shift_percent = X_impact_share_PS
#   }
#   if (DGP == "Y_impact_shift_3") {
#     X_impact_share_Y = X_impact_share_PS
#     X_impact_shift_percent = ceiling(X_impact_share_PS/2)
#   }
# 
#   dataset <- gen_DS_modular(alpha_PS = alpha_PS, X_dim = X_dim, X_impact_share_PS = X_impact_share_PS, Y_impact = Y_impact, cor_X = cor_X, PS_link = PS_link,
#                             X_impact_share_Y = X_impact_share_Y, X_impact_shift_percent = X_impact_shift_percent,
#                             beta_PS = 0.75, min_X = min_X, max_X = max_X)
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridge", "lasso", "bayes_ridge", "rf", "nn")
#   Matching_estimators <- c("nn_one", "nnk")#, "stratum", "caliper_est", "cs")
# 
#   if (mean_diff >= 0.2) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
# 
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   xmatched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = Matching_estimators)
#   TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE)
#   bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
# }
# 
# param_list = list("X_dim" = c(20),
#                   "PS_link" = c("logit"),
#                   "X_impact_share_PS" = c(0.1),
#                   "cor_X" = c(0, 0.85),
#                   "X_range" = c("normal", "small", "large"),
#                   "alpha_PS" = c(-0.7, 0),
#                   "DGP" = c("normal")
# )
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 15,
#                             param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_18.Rdata")

# sim_VM <- function(DGP,
#                    X_dim) {
#   PS_impact = "linear"
#   min_X = -1
#   max_X = 1
#   X_impact_share_PS = 1
#   if(DGP == "quadratic") {PS_impact = "quadratic"}
#   if(DGP == "quadratic_2") {PS_impact = "quadratic_2"}
#   if(DGP == "quadratic_3") {PS_impact = "quadratic_3"}
#   if(DGP == "kink") {PS_impact = "kink"}
#   if(DGP == "kink_2") {PS_impact = "kink_2"}
#   if(DGP == "interaction") {PS_impact = "interaction"
#   X_dim = 4}
#   if(DGP == "X_min_max") {min_X = -1.3
#   max_X = 1.3}
#   if(DGP == "high_dim") {X_dim = 600
#   X_impact_share_PS = 0.01}
#   
#   dataset <- gen_DS_modular(X_dim = X_dim, min_X = min_X, max_X = max_X,
#                             PS_impact = PS_impact, X_impact_share_PS = X_impact_share_PS)
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridge", "lasso", "bayes_ridge", "rf", "nn")
#   Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
#   
#   if (mean_diff >= 0.25) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
#     
#     PS_scores <- PS_estimators(dataset,
#                                estimators = PS_est,
#                                target_var = "T",
#                                vars_to_exclude = c("Y", "PS"))
#     matched_data <- matching_function(PS_scores = PS_scores,
#                                       dataset = dataset,
#                                       matching_estimators = Matching_estimators)
#     TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
#     bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
#   
# }
# 
# param_list <- list("DGP" = c("linear", "quadratic", "quadratic_2", "quadratic_3", "kink",
#                              "kink_2", "X_min_max", "high_dim"),
#                    "X_dim" = c(1, 5))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 25,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_19.Rdata")
# 

# sim_VM <- function(n_obs,
#                    X_dim,
#                    alpha_PS) {
#   X_impact_share_PS = 1
#   if(X_dim == 20) {X_impact_share = 0.25}
#   if(X_dim == 50) {X_impact_share = 0.05}
#   if(X_dim == 100){X_impact_share = 0.025}
#   if(X_dim == 200){X_impact_share = 0.0125}
#   dataset <- gen_DS_modular(X_dim = X_dim, n_obs = n_obs, adjust_beta_PS = TRUE,
#                             alpha_PS = alpha_PS, X_impact_share_PS = X_impact_share_PS)
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridgeNaive", "ridgeInterPoly", "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "rf", "nn")
#   Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
# 
#   if (mean_diff >= 0.25) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
# 
#     PS_scores <- PS_estimators(dataset,
#                                estimators = PS_est,
#                                target_var = "T",
#                                vars_to_exclude = c("Y", "PS"))
#     matched_data <- matching_function(PS_scores = PS_scores,
#                                       dataset = dataset,
#                                       matching_estimators = Matching_estimators)
#     TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
#     bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
# 
# }
# 
# param_list <- list("n_obs" = c(2000),
#                    "X_dim" = c(1, 10, 100),
#                    "alpha_PS" = c(-0.5, 0, 0.3))
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 15,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_19.Rdata")

# sim_VM <- function(DGP, n_obs){
#   PS_impact = "linear"
#   Y_impact = "linear"
#   alpha_PS = -0.2
#   adjust_beta_PS = 0.35
#   if (DGP == "quadratic_linear") {
#     PS_impact = "quadratic"
#     adjust_beta_PS = 0.35}
#   if (DGP == "linear_quadratic") {
#     Y_impact = "quadratic"}
#   if (DGP == "quadratic_quadratic") {
#     Y_impact = "quadratic"
#     PS_impact = "quadratic"
#     adjust_beta_PS = 0.2
#     alpha_PS = -1.6}
# 
#   dataset <- gen_DS_modular(
#     PS_impact = PS_impact, Y_impact = Y_impact, n_obs = n_obs,
#     X_dim = 10, X_impact_share_PS = 0.7, X_covar = 0.5, X_impact_share_Y = 0.7,
#     X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, adjust_alpha_Y = TRUE,
#     alpha_PS = alpha_PS, beta_adjust_power_PS = adjust_beta_PS)
# 
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridgeInterPoly", "lassoInterPoly",
#               "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
#   Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
# 
#   if (mean_diff >= 0.25) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
# 
#     PS_scores <- PS_estimators(dataset,
#                                estimators = PS_est,
#                                target_var = "T",
#                                vars_to_exclude = c("Y", "PS"))
#     matched_data <- matching_function(PS_scores = PS_scores,
#                                       dataset = dataset,
#                                       matching_estimators = Matching_estimators)
#     TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
#     bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
# }
# 
# param_list <- list("n_obs" = c(1000,3000),
#                    "DGP" = c("linear_linear","quadratic_linear",# "quadratic2_linear", "quadratic3_linear",
#                               "linear_quadratic",
#                      "quadratic_quadratic"))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 15,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 4), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_24.Rdata")
#  

# sim_VM <- function(DGP, n_obs){
#   PS_error = FALSE
#   Y_error = FALSE
#   if(DGP == "ps_error") {PS_error = TRUE}
#   if(DGP == "y_error") {Y_error = TRUE}
#   if(DGP == "y_ps_error") {
#     Y_error = TRUE
#     PS_error = TRUE
#   }
# 
# 
#   dataset <- gen_DS_modular(
#     n_obs = n_obs,
#     X_dim = 10, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7,
#     X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.3,
#     adjust_alpha_Y = TRUE,
#     Y_error = Y_error, PS_error = PS_error, alpha_PS = -0.2)
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridgeInterPoly", "lassoInterPoly",
#               "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
#   Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
# 
#   if (mean_diff >= 0.25) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
# 
#     PS_scores <- PS_estimators(dataset,
#                                estimators = PS_est,
#                                target_var = "T",
#                                vars_to_exclude = c("Y", "PS"))
#     matched_data <- matching_function(PS_scores = PS_scores,
#                                       dataset = dataset,
#                                       matching_estimators = Matching_estimators)
#     TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
#     bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
# }
# 
# param_list <- list("n_obs" = c(1000,3000),
#                    "DGP" = c("no_error", "ps_error", "y_error", "y_ps_error"))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 15,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 4), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_22.Rdata")

# sim_VM <- function(covar, n_obs){
#   dataset <- gen_DS_modular(
#     n_obs = n_obs, X_covar = covar, Y_impact = "heterogeneous",
#     X_dim = 10, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7,
#     X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.3,
#     adjust_alpha_Y = TRUE, alpha_PS = -0.2)
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridgeInterPoly", "lassoInterPoly",
#               "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
#   Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
# 
#   if (mean_diff >= 0.25) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
# 
#     PS_scores <- PS_estimators(dataset,
#                                estimators = PS_est,
#                                target_var = "T",
#                                vars_to_exclude = c("Y", "PS"))
#     matched_data <- matching_function(PS_scores = PS_scores,
#                                       dataset = dataset,
#                                       matching_estimators = Matching_estimators)
#     TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
#     bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
# }
# 
# param_list <- list("n_obs" = c(1000,3000),
#                    "covar" = c(0,0.3,0.7))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 15,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 4), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_23.Rdata")

# sim_VM <- function(X_dim){
#   dataset <- gen_DS_modular(
#     n_obs = 1500, X_covar = 0.3,
#     X_dim = X_dim, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7, PS_impact = "range_assign",
#     X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_PS = 0.5, alpha_PS = -1.3, beta_adjust_power_PS = 0.5)
#   #Check for excessive mean diff ins PS scores
#   mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
#   PS_est <- c("true", "log", "probit", "ridgeInterPoly", "lassoInterPoly",
#               "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "rf2", "nn")
#   Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
# 
#   if (mean_diff >= 0.25) {
#     combined_estimators <- c(apply(expand.grid(
#       PS_est, replace(
#         Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
#     bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
#   } else {
# 
#     PS_scores <- PS_estimators(dataset,
#                                estimators = PS_est,
#                                target_var = "T",
#                                vars_to_exclude = c("Y", "PS"))
#     matched_data <- matching_function(PS_scores = PS_scores,
#                                       dataset = dataset,
#                                       matching_estimators = Matching_estimators)
#     TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
#     bias <- lapply(TE_effects, function(x) x - 1)
#   }
#   return(bias)
# }
# 
#   
# param_list <- list("X_dim" = c(20))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 15,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_26.Rdata")

# sim_VM <- function(PS_impact, n_obs){
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   dataset <- gen_DS_modular(n_obs = n_obs, Y_impact = "paper", PS_function = "paper",
#                             PS_impact = PS_impact, PS_link = PS_link, min_X = 0)
#   PS_est <- c("true", "log", "meanT", "zeroOFive")
#   Matching_estimators <- c("nn_one", "nnk")
# 
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   #Drop all observations in dataset and estimated PS_scores with PS not in [0.02,0.98] range of logit estimated scores
#   quantiles <- quantile(PS_scores$log, c(0.02,0.98))
#   indicator <- (PS_scores$log > quantiles[[1]] & PS_scores$log < quantiles[[2]])
#   dataset <- dataset[indicator,]
#   PS_scores <- lapply(PS_scores, function(x) x[indicator])
# 
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = Matching_estimators)
#   TE_effects <- estimate_TE(matched_data, data = dataset, PS_scores = PS_scores,
#                             include_DML = TRUE, include_SimpleReg = TRUE,
#                             include_IPW = TRUE, include_NIPW = TRUE)
#   #Need to already calculate desired PS metrics here and return them as such
#   return(TE_effects)
# }
# 
# 
# param_list <- list("n_obs" = c(100,1000,10000),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 10000,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_35.Rdata")

# sim_VM <- function(PS_impact, n_obs){
#   #Used for estimating variance
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric", 
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   dataset <- gen_DS_modular(n_obs = n_obs, Y_impact = "paper", PS_function = "paper", 
#                             PS_impact = PS_impact, PS_link = PS_link, min_X = 0)
#   PS_est <- c("true", "log", "probit", "meanT", "zeroOFive")
#   Matching_estimators <- c("nn_one", "nnk")
#   
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   #Drop all observations in dataset and estimated PS_scores with PS not in [0.02,0.98] range of logit estimated scores
#   quantiles <- quantile(PS_scores$log, c(0.02,0.98))
#   indicator <- (PS_scores$log > quantiles[[1]] & PS_scores$log < quantiles[[2]])
#   dataset <- dataset[indicator,]
#   PS_scores <- lapply(PS_scores, function(x) x[indicator])
#   
#   matched_data <- matching_function(PS_scores = PS_scores,
#                                     dataset = dataset,
#                                     matching_estimators = Matching_estimators)
#   TE_effects <- estimate_TE(matched_data, data = dataset, PS_scores = PS_scores,
#                             include_DML = TRUE, include_SimpleReg = TRUE,
#                             include_IPW = TRUE, include_NIPW = TRUE)
#   TE <- list(TE_effects$IPW_log, TE_effects$NIPW_log)
#   Var <- lapply(TE, est_var, dataset = dataset, PS_score = PS_scores$log)
#   names(Var) <- c("IPW_Var", "NIPW_VAR")
#   return(Var)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric", 
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic", 
#                                    "stepNonMonotonic"))
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 2000,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_32.Rdata")
# 

# sim_VM <- function(PS_impact, n_obs){
#   #Used for estimating variance
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   dataset <- gen_DS_modular(n_obs = n_obs, Y_impact = "paper", PS_function = "paper",
#                             PS_impact = PS_impact, PS_link = PS_link, min_X = 0)
# 
#   PS_model <- Log_PS_pred(data = dataset, return_Model = TRUE)
# 
#   # MISE <- est_MISE(PS_model_est = PS_model, PS_impact = PS_impact, PS_link = PS_link)
#   # SUP <- est_SUP(PS_model_est = PS_model, PS_impact = PS_impact, PS_link = PS_link)
#   M = 100
#   grid <- data.frame(seq(0,1,0.01), rep(1,M+1))
#   colnames(grid) <- c("X", "OneVector")
#   PS_pred_est <- rep(mean(as.numeric(dataset$T)-1), 101)
#   PS_pred_true <- gen_PS_paper(X = as.matrix(grid[,1]), impact_formula = PS_impact, link_type = PS_link)
#   MISE <- (1/M) * sum((PS_pred_est-PS_pred_true)^2)
#   SUP <- max(abs(PS_pred_est-PS_pred_true))
#   results <- list(MISE, SUP)
#   names(results) <- c("MISE", "SUP")
#   return(results)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 2000,
#                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_34.Rdata")

# sim_VM <- function(PS_impact, n_obs){
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   dataset <- gen_DS_modular(n_obs = n_obs, Y_impact = "paper", PS_function = "paper",
#                             PS_impact = PS_impact, PS_link = PS_link, min_X = 0)
#   Matching_estimators <- c("nn_one")
#   polynomials <- c(0,1,2,3,4,5,6,7,8,9)
#   polynomials_vec <- c()
#   PS_scores <- list()
#   PS_models <- list()
#   for (i in polynomials){
#     polynomials_vec <- c(polynomials_vec, i)
#     PS <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec)
#     PS_scores <- append(PS_scores, list(PS))
#     
#     PS_model <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec, return_Model = TRUE)
#     PS_models <- append(PS_models, list(PS_model))
#   }
#   names(PS_scores) <- polynomials
#   names(PS_models) <- polynomials
#   
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   for (i in polynomials+1){
#     indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
#     if(i>1){ #Because trimming is not working with a constant PS score
#       datasets[[i]] <- dataset[indicator,]
#       PS_scores[[i]] <- PS_scores[[i]][indicator]
#     } else {
#       datasets[[i]] <- dataset
#       PS_scores[[i]] <- PS_scores[[i]]
#     }
#     IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  datasets[[i]])
#     NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = datasets[[i]])
#   }
# 
#   
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PS_impact = PS_impact, PS_link = PS_link)
#   SUPs <- lapply(PS_models, est_SUP, PS_impact = PS_impact, PS_link = PS_link)
#   
#   names(IPW_results) <- paste0("IPW_w_poly", polynomials)
#   names(NIPW_results) <- paste0("NIPW_w_poly", polynomials)
#   names(MISEs) <- paste0("MISE_w_poly_", polynomials)
#   names(SUPs) <- paste0("SUP_w_poly_", polynomials)
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs)
#   #Need to already calculate desired PS metrics here and return them as such
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 5000,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_36.Rdata")

#Get variance
# IPW_VAR_results[[i]] <-  est_var_w_polynomials(
#   dataset = dataset, PS_score = PS_scores[[i]], TE = IPW_results[[i]], poly_degree = i-1)
# NIPW_VAR_results[[i]] <-  est_var_w_polynomials(
#   dataset = dataset, PS_score = PS_scores[[i]], TE = NIPW_results[[i]], poly_degree = i-1)
# 
# #Get coverage ration
# CI <- est_confidence_interval(
#   IPW_VAR_results[[i]], TE = IPW_results[[i]], N = nrow(dataset), confidence_level = 0.975)
# IPW_coverage[[i]] <- (CI[1]<=IPW_results[[i]] & CI[2] >= IPW_results[[i]])
# CI <- est_confidence_interval(
#   NIPW_VAR_results[[i]], TE = NIPW_results[[i]], N = nrow(dataset), confidence_level = 0.975)
# NIPW_coverage[[i]] <- (CI[1]<=NIPW_results[[i]] & CI[2] >= NIPW_results[[i]])
#}


# sim_VM <- function(n_obs){
#   beta_PS = 1
#   beta_adjust_power_PS = 0
#   alpha_PS = -0.5
#   alpha_adjust_power_PS = 0
#   Y_impact = "highdim"
#   X_dim = 1000
#   alpha_outcome = 1/X_dim
#   PS_function = "highdim"
#   X_impact_share_PS = 0.006
#   min_X = 0
#   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, PS_function = PS_function, 
#                             X_impact_share_PS = X_impact_share_PS, min_X = min_X, 
#                             beta_adjust_power_PS = beta_PS, beta_PS = beta_PS,
#                             alpha_adjust_power_PS = alpha_adjust_power_PS, alpha_PS = alpha_PS, 
#                             Y_impact = Y_impact, alpha_outcome = alpha_outcome)
#   
#   PS_scores <- list()
#   PS_models <- list()
#   #####
#   #####
#   #Can speed up computation time by returning model and ps scores in a list in one function call instead of doint it seperately
#   PS_scores$lasso <- Lasso_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE)
#   PS_models$lasso <- Lasso_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                                    return_model = TRUE)
#   PS_scores$ridge <- Ridge_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE)
#   PS_models$ridge <- Ridge_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                                    return_model = TRUE)
#   
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   IPW_VAR_results <-list()
#   NIPW_VAR_results <-list()
#   IPW_coverage <- list()
#   NIPW_coverage <- list()
#   
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   indicator_lasso <- PS_scores$lasso > quantiles_list[[1]][[1]] & PS_scores$lasso < quantiles_list[[1]][[2]]
#   indicator_ridge <- PS_scores$ridge > quantiles_list[[2]][[1]] & PS_scores$ridge < quantiles_list[[2]][[2]]
#   datasets$lasso <- dataset[indicator_lasso,]
#   datasets$ridge <- dataset[indicator_ridge,]
#   PS_scores$lasso <- PS_scores$lasso[indicator_lasso]
#   PS_scores$ridge <- PS_scores$ridge[indicator_ridge]
#   
#   IPW_results$lasso <- IPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   IPW_results$ridge <- IPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   NIPW_results$lasso <- NIPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   NIPW_results$ridge <- NIPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   
#   
#   #Get variance
#   IPW_VAR_results$lasso <-  est_var(
#     dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = IPW_results$lasso)
#   IPW_VAR_results$ridge <-  est_var(
#     dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = IPW_results$ridge)
#   NIPW_VAR_results$lasso <-  est_var(
#     dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = NIPW_results$lasso)
#   NIPW_VAR_results$ridge <-  est_var(
#     dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = NIPW_results$ridge)
#   
#   #Get coverage ration
#   CI <- est_confidence_interval(
#     IPW_VAR_results$lasso, TE = IPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   IPW_coverage$lasso <- (CI[1]<=IPW_results$lasso & CI[2] >= IPW_results$lasso)
#   CI <- est_confidence_interval(
#     IPW_VAR_results$ridge, TE = IPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   IPW_coverage$ridge <- (CI[1]<=IPW_results$ridge & CI[2] >= IPW_results$ridge)
#   CI <- est_confidence_interval(
#     NIPW_VAR_results$lasso, TE = NIPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   NIPW_coverage$lasso <- (CI[1]<=NIPW_results$lasso & CI[2] >= NIPW_results$lasso)
#   CI <- est_confidence_interval(
#     NIPW_VAR_results$ridge, TE = NIPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   NIPW_coverage$ridge <- (CI[1]<=NIPW_results$ridge & CI[2] >= NIPW_results$ridge)
#   
#   
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PSfun = "highdim", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                   alpha_adjust_power = alpha_adjust_power_PS, alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge")
#   SUPs <- lapply(PS_models, est_SUP, PSfun = "highdim", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                  alpha_adjust_power = alpha_adjust_power_PS, alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge")
#   
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   names(MISEs) <- paste0("MISE_", names(MISEs))
#   names(SUPs) <- paste0("SUP_", names(SUPs))
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, NIPW_VAR_results, 
#                  IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(500, 1000, 1500, 2000, 5000))
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 5000,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_40.Rdata")

# 
# sim_VM <- function(n_obs, PS_impact){
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {
#     PS_link = "logit"}
#   beta_PS = 1
#   beta_adjust_power_PS = 0
#   alpha_PS = 1
#   alpha_adjust_power_PS = 0
#   Y_impact = "highdim"
#   X_dim = 1000
#   alpha_outcome = 1/X_dim
#   PS_function = "paper"
#   X_impact_share_PS = 0.006
#   min_X = 0
#   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, PS_function = PS_function, PS_impact = PS_impact,
#                             X_impact_share_PS = X_impact_share_PS, min_X = min_X,
#                             beta_adjust_power_PS = beta_adjust_power_PS, beta_PS = beta_PS,
#                             alpha_adjust_power_PS = alpha_adjust_power_PS, alpha_PS = alpha_PS,
#                             Y_impact = Y_impact, alpha_outcome = alpha_outcome, PS_link = PS_link)
#   
#   PS_scores <- list()
#   PS_models <- list()
#   #####
#   #####
#   output <- Lasso_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                                    return_modelAndPS = TRUE)
#   PS_scores$lasso <- output[[1]]
#   PS_models$lasso <- output[[2]]
# 
#   output <- Ridge_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$ridge <- output[[1]]
#   PS_models$ridge <- output[[2]]
# 
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   IPW_VAR_results <-list()
#   NIPW_VAR_results <-list()
#   IPW_coverage <- list()
#   NIPW_coverage <- list()
#   PS_constant <- list("lasso_constant"=0, "ridge_constant"=0)
# 
#   #quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   indicator_lasso <- PS_scores$lasso > quantiles_list[[1]][[1]] & PS_scores$lasso < quantiles_list[[1]][[2]]
#   indicator_ridge <- PS_scores$ridge > quantiles_list[[2]][[1]] & PS_scores$ridge < quantiles_list[[2]][[2]]
#   if (sum(indicator_lasso) == 0) {PS_constant$lasso_constant = 1}
#   if (mean(indicator_lasso) < 0.95) {indicator_lasso <- rep(TRUE, length(indicator_lasso))}
#   if (sum(indicator_ridge) == 0) {PS_constant$ridge_constant = 1}
#   if (mean(indicator_ridge) < 0.95) {indicator_ridge <- rep(TRUE, length(indicator_ridge))}
#   
#   #Don't trim in case of flat dataset, it might cut off the whole dataset
#   datasets$lasso <- dataset[indicator_lasso,1:ncol(dataset)]
#   datasets$ridge <- dataset[indicator_ridge,1:ncol(dataset)] 
#   PS_scores$lasso <- PS_scores$lasso[indicator_lasso]
#   PS_scores$ridge <- PS_scores$ridge[indicator_ridge]
# 
#   IPW_results$lasso <- IPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   IPW_results$ridge <- IPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   NIPW_results$lasso <- NIPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   NIPW_results$ridge <- NIPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
# 
# 
#   #Get variance
#   # IPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = IPW_results$lasso)
#   # IPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = IPW_results$ridge)
#   # NIPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = NIPW_results$lasso)
#   # NIPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = NIPW_results$ridge)
# 
#   #Get coverage ration
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$lasso, TE = IPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$lasso <- (CI[1]<=IPW_results$lasso & CI[2] >= IPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$ridge, TE = IPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$ridge <- (CI[1]<=IPW_results$ridge & CI[2] >= IPW_results$ridge)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$lasso, TE = NIPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$lasso <- (CI[1]<=NIPW_results$lasso & CI[2] >= NIPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$ridge, TE = NIPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$ridge <- (CI[1]<=NIPW_results$ridge & CI[2] >= NIPW_results$ridge)
# 
# 
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS, 
#                   PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS, 
#                   alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
#   SUPs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS, 
#                  PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS, 
#                  alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
# 
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   # names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   # names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   # names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   # names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   names(MISEs) <- paste0("MISE_", names(MISEs))
#   names(SUPs) <- paste0("SUP_", names(SUPs))
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, PS_constant)#, NIPW_VAR_results,
#   #IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(500, 1500),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 100,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# 
# save(sim_VM_result, file = "sim_VM_41.Rdata")

# sim_VM <- function(n_obs, PS_impact, X_dim, X_impact_share_Y){
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   beta_PS = 1
#   beta_adjust_power_PS = 0
#   alpha_PS = 1
#   alpha_adjust_power_PS = 0
#   Y_impact = "highdim"
#   #X_dim = 1000
#   alpha_outcome = (1/(X_dim*X_impact_share_Y))
#   PS_function = "paper"
#   X_impact_share_PS = 6/X_dim
#   min_X = 0
#   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, PS_function = PS_function, PS_impact = PS_impact,
#                             X_impact_share_PS = X_impact_share_PS, min_X = min_X, PS_link = PS_link,
#                             beta_adjust_power_PS = beta_adjust_power_PS, beta_PS = beta_PS,
#                             alpha_adjust_power_PS = alpha_adjust_power_PS, alpha_PS = alpha_PS,
#                             Y_impact = Y_impact, alpha_outcome = alpha_outcome, X_impact_share_Y = X_impact_share_Y)
#   PS_scores <- list()
#   PS_models <- list()
#   #####
#   #####
#   output <- Lasso_PS_pred(data = dataset, include_interactions = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$lasso <- output[[1]]
#   PS_models$lasso <- output[[2]]
# 
#   output <- Ridge_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$ridge <- output[[1]]
#   PS_models$ridge <- output[[2]]
# 
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   IPW_VAR_results <-list()
#   NIPW_VAR_results <-list()
#   IPW_coverage <- list()
#   NIPW_coverage <- list()
#   PS_constant <- list("lasso_constant"=0, "ridge_constant"=0)
# 
#   #quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   quantiles_list <- lapply(PS_scores, quantile, c(0.02,0.98))
#   indicator_lasso <- PS_scores$lasso > quantiles_list[[1]][[1]] & PS_scores$lasso < quantiles_list[[1]][[2]]
#   indicator_ridge <- PS_scores$ridge > quantiles_list[[2]][[1]] & PS_scores$ridge < quantiles_list[[2]][[2]]
#   if (sum(indicator_lasso) == 0) {PS_constant$lasso_constant = 1}
#   if (mean(indicator_lasso) < 0.95) {indicator_lasso <- rep(TRUE, length(indicator_lasso))}
#   if (sum(indicator_ridge) == 0) {PS_constant$ridge_constant = 1}
#   if (mean(indicator_ridge) < 0.95) {indicator_ridge <- rep(TRUE, length(indicator_ridge))}
# 
#   #Don't trim in case of flat dataset, it might cut off the whole dataset
#   datasets$lasso <- dataset[indicator_lasso,1:ncol(dataset)]
#   datasets$ridge <- dataset[indicator_ridge,1:ncol(dataset)]
#   PS_scores$lasso <- PS_scores$lasso[indicator_lasso]
#   PS_scores$ridge <- PS_scores$ridge[indicator_ridge]
# 
#   IPW_results$lasso <- IPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   IPW_results$ridge <- IPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   NIPW_results$lasso <- NIPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   NIPW_results$ridge <- NIPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
# 
# 
#   #Get variance
#   # IPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = IPW_results$lasso)
#   # IPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = IPW_results$ridge)
#   # NIPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = NIPW_results$lasso)
#   # NIPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = NIPW_results$ridge)
# 
#   #Get coverage ration
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$lasso, TE = IPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$lasso <- (CI[1]<=IPW_results$lasso & CI[2] >= IPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$ridge, TE = IPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$ridge <- (CI[1]<=IPW_results$ridge & CI[2] >= IPW_results$ridge)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$lasso, TE = NIPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$lasso <- (CI[1]<=NIPW_results$lasso & CI[2] >= NIPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$ridge, TE = NIPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$ridge <- (CI[1]<=NIPW_results$ridge & CI[2] >= NIPW_results$ridge)
# 
# 
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                   PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS,
#                   alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
#   SUPs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                  PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS,
#                  alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
# 
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   # names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   # names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   # names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   # names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   names(MISEs) <- paste0("MISE_", names(MISEs))
#   names(SUPs) <- paste0("SUP_", names(SUPs))
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, PS_constant)#, NIPW_VAR_results,
#   #IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "X_dim" = c(1000),
#                    "X_impact_share_Y" = c(0.001,0.01,0.1,0.5,1),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
#                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 100,
# #                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_44.Rdata")

# sim_VM <- function(n_obs, PS_impact, X_impact_share_PS){
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   beta_PS = 1
#   beta_adjust_power_PS = 0
#   alpha_PS = 1
#   alpha_adjust_power_PS = 0
#   Y_impact = "highdim"
#   X_dim = 1000
#   alpha_outcome = 1/X_dim
#   PS_function = "paper"
#   #X_impact_share_PS = 0.006
#   min_X = 0
#   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, PS_function = PS_function, PS_impact = PS_impact,
#                             X_impact_share_PS = X_impact_share_PS, min_X = min_X, PS_link = PS_link,
#                             beta_adjust_power_PS = beta_adjust_power_PS, beta_PS = beta_PS,
#                             alpha_adjust_power_PS = alpha_adjust_power_PS, alpha_PS = alpha_PS,
#                             Y_impact = Y_impact, alpha_outcome = alpha_outcome)
#   PS_scores <- list()
#   PS_models <- list()
#   #####
#   #####
#   output <- Lasso_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$lasso <- output[[1]]
#   PS_models$lasso <- output[[2]]
# 
#   output <- Ridge_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$ridge <- output[[1]]
#   PS_models$ridge <- output[[2]]
# 
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   IPW_VAR_results <-list()
#   NIPW_VAR_results <-list()
#   IPW_coverage <- list()
#   NIPW_coverage <- list()
#   PS_constant <- list("lasso_constant"=0, "ridge_constant"=0)
# 
#   #quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   indicator_lasso <- PS_scores$lasso > quantiles_list[[1]][[1]] & PS_scores$lasso < quantiles_list[[1]][[2]]
#   indicator_ridge <- PS_scores$ridge > quantiles_list[[2]][[1]] & PS_scores$ridge < quantiles_list[[2]][[2]]
#   if (sum(indicator_lasso) == 0) {PS_constant$lasso_constant = 1}
#   if (mean(indicator_lasso) < 0.95) {indicator_lasso <- rep(TRUE, length(indicator_lasso))}
#   if (sum(indicator_ridge) == 0) {PS_constant$ridge_constant = 1}
#   if (mean(indicator_ridge) < 0.95) {indicator_ridge <- rep(TRUE, length(indicator_ridge))}
# 
#   #Don't trim in case of flat dataset, it might cut off the whole dataset
#   datasets$lasso <- dataset[indicator_lasso,1:ncol(dataset)]
#   datasets$ridge <- dataset[indicator_ridge,1:ncol(dataset)]
#   PS_scores$lasso <- PS_scores$lasso[indicator_lasso]
#   PS_scores$ridge <- PS_scores$ridge[indicator_ridge]
# 
#   IPW_results$lasso <- IPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   IPW_results$ridge <- IPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   NIPW_results$lasso <- NIPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   NIPW_results$ridge <- NIPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
# 
# 
#   #Get variance
#   # IPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = IPW_results$lasso)
#   # IPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = IPW_results$ridge)
#   # NIPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = NIPW_results$lasso)
#   # NIPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = NIPW_results$ridge)
# 
#   #Get coverage ration
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$lasso, TE = IPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$lasso <- (CI[1]<=IPW_results$lasso & CI[2] >= IPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$ridge, TE = IPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$ridge <- (CI[1]<=IPW_results$ridge & CI[2] >= IPW_results$ridge)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$lasso, TE = NIPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$lasso <- (CI[1]<=NIPW_results$lasso & CI[2] >= NIPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$ridge, TE = NIPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$ridge <- (CI[1]<=NIPW_results$ridge & CI[2] >= NIPW_results$ridge)
# 
# 
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                   PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS,
#                   alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
#   SUPs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                  PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS,
#                  alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
# 
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   # names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   # names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   # names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   # names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   names(MISEs) <- paste0("MISE_", names(MISEs))
#   names(SUPs) <- paste0("SUP_", names(SUPs))
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, PS_constant)#, NIPW_VAR_results,
#   #IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "X_impact_share_PS" = c(0.001, 0.003, 0.006, 0.009, 0.012),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 100,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_43.Rdata")



# sim_VM <- function(n_obs, PS_impact, X_dim){
#   PS_link = "raw"
#   if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   beta_PS = 1
#   beta_adjust_power_PS = 0
#   alpha_PS = 1
#   alpha_adjust_power_PS = 0
#   Y_impact = "highdim"
#   #X_dim = 1000
#   alpha_outcome = 1/X_dim
#   PS_function = "paper"
#   X_impact_share_PS = 6/X_dim
#   min_X = 0
#   dataset <- gen_DS_modular(n_obs = n_obs, X_dim = X_dim, PS_function = PS_function, PS_impact = PS_impact,
#                             X_impact_share_PS = X_impact_share_PS, min_X = min_X, PS_link = PS_link,
#                             beta_adjust_power_PS = beta_adjust_power_PS, beta_PS = beta_PS,
#                             alpha_adjust_power_PS = alpha_adjust_power_PS, alpha_PS = alpha_PS, 
#                             Y_impact = Y_impact, alpha_outcome = alpha_outcome)
#   PS_scores <- list()
#   PS_models <- list()
#   #####
#   #####
#   output <- Lasso_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$lasso <- output[[1]]
#   PS_models$lasso <- output[[2]]
#   
#   output <- Ridge_PS_pred(data = dataset, include_interactions = FALSE, include_polynomials = FALSE,
#                           return_modelAndPS = TRUE)
#   PS_scores$ridge <- output[[1]]
#   PS_models$ridge <- output[[2]]
#   
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   IPW_VAR_results <-list()
#   NIPW_VAR_results <-list()
#   IPW_coverage <- list()
#   NIPW_coverage <- list()
#   PS_constant <- list("lasso_constant"=0, "ridge_constant"=0)
#   
#   #quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#   indicator_lasso <- PS_scores$lasso > quantiles_list[[1]][[1]] & PS_scores$lasso < quantiles_list[[1]][[2]]
#   indicator_ridge <- PS_scores$ridge > quantiles_list[[2]][[1]] & PS_scores$ridge < quantiles_list[[2]][[2]]
#   if (sum(indicator_lasso) == 0) {PS_constant$lasso_constant = 1}
#   if (mean(indicator_lasso) < 0.95) {indicator_lasso <- rep(TRUE, length(indicator_lasso))}
#   if (sum(indicator_ridge) == 0) {PS_constant$ridge_constant = 1}
#   if (mean(indicator_ridge) < 0.95) {indicator_ridge <- rep(TRUE, length(indicator_ridge))}
#   
#   #Don't trim in case of flat dataset, it might cut off the whole dataset
#   datasets$lasso <- dataset[indicator_lasso,1:ncol(dataset)]
#   datasets$ridge <- dataset[indicator_ridge,1:ncol(dataset)] 
#   PS_scores$lasso <- PS_scores$lasso[indicator_lasso]
#   PS_scores$ridge <- PS_scores$ridge[indicator_ridge]
#   
#   IPW_results$lasso <- IPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   IPW_results$ridge <- IPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   NIPW_results$lasso <- NIPW_fun(PS_est = PS_scores$lasso, dataset =  datasets$lasso)
#   NIPW_results$ridge <- NIPW_fun(PS_est = PS_scores$ridge, dataset =  datasets$ridge)
#   
#   
#   #Get variance
#   # IPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = IPW_results$lasso)
#   # IPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = IPW_results$ridge)
#   # NIPW_VAR_results$lasso <-  est_var(
#   #   dataset = datasets$lasso, PS_score = PS_scores$lasso, TE = NIPW_results$lasso)
#   # NIPW_VAR_results$ridge <-  est_var(
#   #   dataset = datasets$ridge, PS_score = PS_scores$ridge, TE = NIPW_results$ridge)
#   
#   #Get coverage ration
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$lasso, TE = IPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$lasso <- (CI[1]<=IPW_results$lasso & CI[2] >= IPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   IPW_VAR_results$ridge, TE = IPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # IPW_coverage$ridge <- (CI[1]<=IPW_results$ridge & CI[2] >= IPW_results$ridge)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$lasso, TE = NIPW_results$lasso, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$lasso <- (CI[1]<=NIPW_results$lasso & CI[2] >= NIPW_results$lasso)
#   # CI <- est_confidence_interval(
#   #   NIPW_VAR_results$ridge, TE = NIPW_results$ridge, N = nrow(dataset), confidence_level = 0.975)
#   # NIPW_coverage$ridge <- (CI[1]<=NIPW_results$ridge & CI[2] >= NIPW_results$ridge)
#   
#   
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS, 
#                   PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS, 
#                   alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
#   SUPs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS, 
#                  PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS, 
#                  alpha = alpha_PS, cols = X_dim, model_type = "lasso/ridge", impact_share = X_impact_share_PS)
#   
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   # names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   # names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   # names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   # names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   names(MISEs) <- paste0("MISE_", names(MISEs))
#   names(SUPs) <- paste0("SUP_", names(SUPs))
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, PS_constant)#, NIPW_VAR_results,
#   #IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "X_dim" = c(10, 500, 1000, 3000),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# 
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 100,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_42.Rdata")



