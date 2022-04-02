# Load required packages
packages <- c("tidyverse","tidymodels", "glmnet", "workflows", "tune")
lapply(packages, require, character.only = TRUE)

###
# Implement RF
###

RF_PS_pred <- function(data, 
                       target_var = "T", 
                       vars_to_exclude = c("Y", "PS_scaled"),
                       n_trees_grid = c(20,40,100),
                       mtry_grid = c(3,4,5,6),
                       min_n_grid = c(5,10,20)
) {
  rf <- rand_forest(mode = "classification",
                    engine = "ranger",
                    mtry = tune(),
                    trees = tune(),
                    min_n = tune()) 
  
  #Specify parameter grid
  rf_grid <- expand.grid(mtry = mtry_grid,
                         trees = n_trees_grid,
                         min_n = min_n_grid)
  
  #Creating cross-validation of dataset
  data_cv <- vfold_cv(data)
  
  #Define recipe (which includes pre-processing and the regression formula)
  if (is.na(vars_to_exclude)) {cols = colnames(data)} else {cols = colnames(data[!names(data) %in% vars_to_exclude])}
  
  recipe <- recipe(data[,cols]) %>%
    update_role(target_var, new_role = "outcome") %>%
    update_role(-target_var, new_role = "predictor") %>%
    step_mutate(target_var = as.factor(target_var))
  
  #Combine in a workflow
  rf_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf)
  
  #Tune Hyperparameters
  rf_tune_results <- rf_workflow %>%
    tune_grid(resamples = data_cv,
              grid = rf_grid)#, 
  #            metrics = metric_set(accuracy, roc_auc))
  
  #Extract best parameters
  param_final <- rf_tune_results %>% 
    select_best(metric = "accuracy")
  
  #Use best parameters
  rf_workflow <- rf_workflow %>% finalize_workflow(param_final)
  
  #Fit final model
  final_model <- fit(rf_workflow, data)
  
  #Predict (on existing data)
  RF_PS <- predict(final_model, new_data = data, type = "prob")$.pred_1
  return(RF_PS)
}

###
# Lasso, Ridge, "normal" logistic regression
###

Lasso_PS_pred <- function(data, 
                          target_var = "T", 
                          vars_to_exclude = c("Y", "PS_scaled"), 
                          family = "binomial") {
  drop_vars <- c(target_var, vars_to_exclude)
  X <- as.matrix(data[,!names(data) %in% drop_vars])
  T <- as.matrix(data[,target_var])
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = family)
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
  PS_pred_lasso <- as.vector(predict(object = log_lasso_reg, newx = X, type = "response"))
  return(PS_pred_lasso)
}

Ridge_PS_pred <- function(data, 
                          target_var = "T", 
                          vars_to_exclude = c("Y", "PS_scaled"), 
                          family = "binomial") {
  drop_vars <- c(target_var, vars_to_exclude)
  X <- as.matrix(data[,!names(data) %in% drop_vars])
  T <- as.matrix(data[,target_var])
  cv.ridge <- cv.glmnet(x = X, y = T, alpha = 0, family = family)
  log_ridge_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 0, lambda = cv.ridge$lambda.min) 
  PS_pred_ridge <- as.vector(predict(object = log_ridge_reg, newx = X, type = "response"))
  return(PS_pred_ridge)
}

Log_PS_pred <- function(data, 
                        target_var = "T",
                        vars_to_exclude = c("Y", "PS_scaled")) {
  drop_vars <- c(target_var, vars_to_exclude)
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- paste(target_var, "~", paste(regressors, collapse = "+"))
  log_reg <- glm(f, data = data, family = binomial(link = "logit"))
  PS_pred_log <- as.vector(predict.glm(object = log_reg, newdata = data, type = "response"))
  return(PS_pred_log)
}

###
# Combining PS estimators 
###

PS_estimators <- function(data,
                          target_var = "T",
                          vars_to_exclude = c("Y", "PS_scaled"),
                          estimators = c("log", "ridge", "lasso", "rf")) {
  #Function takes data, uses all matching estimators specified and returns list with estimated propensity scores
  results <- list() 
  if ("log" %in% estimators) {
    results <- append(results, list("log" = 
                                      Log_PS_pred(data, target_var, vars_to_exclude)))}
  if ("ridge" %in% estimators) {
    results <- append(results, list("ridge" = 
                                      Ridge_PS_pred(data, target_var, vars_to_exclude)))}
  if ("lasso" %in% estimators) {
    results <- append(results, list("lasso" = 
                                      Lasso_PS_pred(data, target_var, vars_to_exclude)))}
  if ("rf" %in% estimators) {
    results <- append(results, list("rf" = 
                                      RF_PS_pred(data, target_var, vars_to_exclude)))}
  return(results)
}

###
# Estimate TE
###
#This should estimate treatment effects given all combinations of matches
estimate_TE <- function(list_of_matches){
  keys = c()
  reg_results = c()
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
  results <- setNames(as.list(reg_results), keys)
  return(results)
}