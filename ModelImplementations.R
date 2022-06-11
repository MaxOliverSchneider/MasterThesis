# Load required packages
packages <- c("tidyverse","tidymodels", "glmnet", "workflows", "tune", "magrittr", 
              "mlr3", "mlr3learners", "data.table", "bayesreg", "keras", "DoubleML", "randomForest")
lapply(packages, require, character.only = TRUE)

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


###
# Lasso, Ridge, "normal" logistic & probit regression plus true PS value
###

LassoNaive_PS_pred <- function(data, 
                               target_var = "T", 
                               vars_to_exclude = c("Y", "PS"), 
                               family = "binomial") {
  drop_vars <- c(target_var, vars_to_exclude)
  X <- as.matrix(data[,!names(data) %in% drop_vars])
  T <- as.matrix(data[,target_var])
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = family, nfolds = 5)
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
  PS_pred_lasso <- as.vector(predict(object = log_lasso_reg, newx = X, type = "response"))
  return(PS_pred_lasso)
}

Lasso_PS_pred <- function(data, 
                          target_var = "T", 
                          include_interactions = TRUE,
                          include_polynomials = TRUE,
                          vars_to_exclude = c("Y", "PS"), 
                          family = "binomial",
                          return_model = FALSE) {
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
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = family, nfolds = 5)
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
  PS_pred_lasso <- as.vector(predict(object = log_lasso_reg, newx = X, type = "response"))
  if(return_model){PS_pred_lasso = log_lasso_reg}
  return(PS_pred_lasso)
}

Ridge_PS_pred <- function(data, 
                          target_var = "T",
                          include_interactions = TRUE,
                          include_polynomials = TRUE,
                          vars_to_exclude = c("Y", "PS"), 
                          family = "binomial",
                          return_model = FALSE) {
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
  if (return_model){PS_pred_ridge <- log_ridge_reg}
  return(PS_pred_ridge)
}

Log_PS_pred <- function(data,
                        polynomials_vector = NA,
                        target_var = "T",
                        vars_to_exclude = c("Y", "PS"),
                        return_Model = FALSE) {
  drop_vars <- c(target_var, vars_to_exclude)
  regressors <- names(data)[!names(data) %in% drop_vars]
  if(!is.na(polynomials_vector)){
    regressors <- regressors[regressors!="OneVector"]
    regressors <- c(paste0("I(",regressors,"^",polynomials_vector,")"),"OneVector")
  }
  f <- paste(target_var, "~", paste(regressors, collapse = "+"))
  log_reg <- glm(f, data = data, family = binomial(link = "logit"))
  PS_pred_log <- as.vector(predict.glm(object = log_reg, newdata = data, type = "response"))
  if (return_Model){PS_pred_log <- log_reg}
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
}

###
# Combining PS estimators 
###

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

###
# Estimate variance as in Millimet & Tchernis (2009)
###
est_var <- function(dataset, PS_score, TE){
  T <- as.numeric(dataset$T)-1
  Y <- dataset$Y
  X <- dataset$X
  N <- nrow(dataset)
  pheta_vec <- ((Y*T)/PS_score) - ((Y*(1-T))/(1-PS_score)) - TE
  alpha_part_one <- (1/N)*sum((((Y*T)/PS_score^2) + ((Y*(1-T))/((1-PS_score)^2)))*X)
  alpha_part_two <- (1/N)*sum(X^2) 
  alpha_part_three <- X*(T-PS_score)
   alpha_vec <- -1*(alpha_part_one/alpha_part_two) * alpha_part_three
  V_est <- (1/N)*sum((pheta_vec+alpha_vec)^2)
  return(V_est)
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

est_confidence_interval <- function(var_est, TE, N, confidence_level = 0.975){
  Z <- qnorm(confidence_level)
  upper <- TE + Z * (var_est^0.5) / (N^0.5)
  lower <- TE - Z * (var_est^0.5) / (N^0.5)
  return(c(lower, upper))
}

###
# Estimate MISE
###
est_MISE <- function(PS_model_est, PS_impact = "linear", PS_link ="logit", cols = 1,  model_type = "logit", PSfun = "paper",
                     impact_share = 1, beta = 6, beta_adjust_power = 0,
                     alpha = -3, alpha_adjust_power = 0){
  M = 100
  grid <- data.frame(matrix(rep(seq(0,1,0.01), cols), ncol = cols), rep(1,M+1))
  numbers <- seq(1,cols,1)
  colnames(grid) <- c(paste0("X", numbers), "OneVector")
  if (model_type == "logit"){
    PS_pred_est <- predict.glm(PS_model_est, newdata = grid, type = "response")
  } else if (model_type == "lasso/ridge"){
    PS_pred_est <- as.vector(predict(object = PS_model_est, newx = as.matrix(grid), type = "response"))
  }
  if (PSfun == "paper"){
    PS_pred_true <- gen_PS_paper(X = as.matrix(grid[,-cols-1]), impact_formula = PS_impact, link_type = PS_link)
  } else if (PSfun == "highdim")
    PS_pred_true <- gen_PS_highDim(X = as.matrix(grid[,-cols-1]), impact_share = impact_share, beta = beta,
                                   beta_adjust_power = beta_adjust_power, alpha = alpha, alpha_adjust_power = alpha_adjust_power)
  
  MISE <- (1/M) * sum((PS_pred_est-PS_pred_true)^2)
  return(MISE)
}

 est_SUP <- function(PS_model_est, PS_impact = "linear", PS_link ="logit", cols = 1,  model_type = "logit", PSfun = "paper",
                     impact_share = 1, beta = 6, beta_adjust_power = 0,
                     alpha = -3, alpha_adjust_power = 0){
   M = 100
   grid <- data.frame(matrix(rep(seq(0,1,0.01), cols), ncol = cols), rep(1,M+1))
   numbers <- seq(1,cols,1)
   colnames(grid) <- c(paste0("X", numbers), "OneVector")
   if (model_type == "logit"){
     PS_pred_est <- predict.glm(PS_model_est, newdata = grid, type = "response")
   } else if (model_type == "lasso/ridge"){
     PS_pred_est <- as.vector(predict(object = PS_model_est, newx = as.matrix(grid), type = "response"))
   }
   if (PSfun == "paper"){
     PS_pred_true <- gen_PS_paper(X = as.matrix(grid[,-cols-1]), impact_formula = PS_impact, link_type = PS_link)
   } else if (PSfun == "highdim")
     PS_pred_true <- gen_PS_highDim(X = as.matrix(grid[,-cols-1]), impact_share = impact_share, beta = beta,
                                    beta_adjust_power = beta_adjust_power, alpha = alpha, alpha_adjust_power = alpha_adjust_power)
   
   SUP <- max(abs(PS_pred_est-PS_pred_true))
   return(SUP)
}

###
# Check predictive performance of PS estimators
###

PS_predictive_performance <- function(dataset, PS_scores, printLatex = FALSE) {
  performance = lapply(PS_scores, function(i) 
    sum(ifelse(i>0.5, 1, 0) == dataset$T)/nrow(dataset))
  if(printLatex) {print(xtable(data.frame(performance), type = "latex"))}
  return(data.frame(performance))
}

###
# Check whether predicted propensity scores are constant or close to it
###

check_PS_pred_dist <- function(PS_score) {
  ifelse(max(PS_score)-min(PS_score)<0.1, 1,0)
}


###
# Testing
###
# PS_scores <- PS_estimators(dataset,
#                            estimators = c("true", "nn"),
#                            target_var = "T",
#                            vars_to_exclude = c("Y", "PS"))
# 
# easy_NN y- function(epochs, hidden_units, dropout){
# nn <- mlp(epochs = epochs,
#   #penalty = tune(),
#   hidden_units = hidden_units, 
#   dropout = dropout %>%
#   set_mode("classification") %>%
#   set_engine("keras", verbose  = 0)
# 
# nn_recipe <- recipe(f, data = data) %>%
#   #step_BoxCox(all_predictors())%>%
#   step_normalize(all_predictors()) %>%
#   prep(training = data, retain = TRUE)
# 
# nn_workflow <- workflow() %>% add_recipe(nn_recipe) %>% add_model(nn)
# 
# nn_tune_results <- nn_workflow %>% tune_grid(resamples = data_cv,
#                                              grid = nn_grid,
#                                              metrics = metric_set(accuracy, roc_auc))
# #Extract best parameters
# param_final <- nn_tune_results %>% 
#   select_best(metric = "accuracy")
# 
# #Use best parameters
# nn_workflow <- nn_workflow %>% finalize_workflow(param_final)
# 
# #Fit final model
# final_model <- fit(nn_workflow, data)
# 
# #Predict (on existing data)
# NN_PS <- predict(final_model, new_data = data, type = "prob")$.pred_1

#data <- gen_DS_modular(n_obs = 500, PS_link = "probit", X_dim = 600, X_impact_share = 0.5)

