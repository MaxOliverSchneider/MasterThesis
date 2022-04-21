# Load required packages
packages <- c("tidyverse","tidymodels", "glmnet", "workflows", "tune", "magrittr", "DoubleML",
              "mlr3", "mlr3learners", "data.table", "bayesreg")
lapply(packages, require, character.only = TRUE)

###
# Implementation of RF
###
#Needs some more thinking about parameter grid

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
  
  #Predict (on existing data)
  RF_PS <- predict(final_model, new_data = data, type = "prob")$.pred_1
  return(RF_PS)
}

###
# Bayesian Regression (ridge, lasso, horseshoe, horseshoe+)
###
#https://cran.r-project.org/web/packages/bayesreg/bayesreg.pdf

Bayes_Ridge_PS_pred <- function(data, 
                        target_var = "T",
                        vars_to_exclude = c("Y", "PS_scaled")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- as.formula(paste(target_var, "~", paste(regressors, collapse = "+")))
  bayes_ridge_reg <- bayesreg(f, data = data, model = "logistic", prior = "ridge")
  PS_pred_bayes_ridge <- as.vector(predict(object = bayes_ridge_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_ridge)
}

Bayes_Lasso_PS_pred <- function(data, 
                                target_var = "T",
                                vars_to_exclude = c("Y", "PS_scaled")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- as.formula(paste(target_var, "~", paste(regressors, collapse = "+")))
  bayes_lasso_reg <- bayesreg(f, data = data, model = "logistic", prior = "lasso")
  PS_pred_bayes_lasso <- as.vector(predict(object = bayes_lasso_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_lasso)
}

Bayes_Horseshoe_PS_pred <- function(data, 
                                target_var = "T",
                                vars_to_exclude = c("Y", "PS_scaled")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- as.formula(paste(target_var, "~", paste(regressors, collapse = "+")))
  bayes_horseshoe_reg <- bayesreg(f, data = data, model = "logistic", prior = "horseshoe")
  PS_pred_bayes_horseshoe <- as.vector(predict(object = bayes_horseshoe_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_horseshoe)
}

Bayes_HorseshoePlus_PS_pred <- function(data, 
                                    target_var = "T",
                                    vars_to_exclude = c("Y", "PS_scaled")) {
  drop_vars <- c(target_var, vars_to_exclude, "OneVector") #Need to additionally drop the one vector since it causes complications in the bayesian models
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- as.formula(paste(target_var, "~", paste(regressors, collapse = "+")))
  bayes_horseshoePlus_reg <- bayesreg(f, data = data, model = "logistic", prior = "horseshoe+")
  PS_pred_bayes_horseshoePlus <- as.vector(predict(object = bayes_horseshoePlus_reg, newdata = data, type = "response"))
  return(PS_pred_bayes_horseshoePlus)
}


###
# Lasso, Ridge, "normal" logistic & probit regression plus true PS value
###

Lasso_PS_pred <- function(data, 
                          target_var = "T", 
                          vars_to_exclude = c("Y", "PS_scaled"), 
                          family = "binomial") {
  drop_vars <- c(target_var, vars_to_exclude)
  X <- as.matrix(data[,!names(data) %in% drop_vars])
  T <- as.matrix(data[,target_var])
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = family, nfolds = 5)
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
  cv.ridge <- cv.glmnet(x = X, y = T, alpha = 0, family = family, nfolds = 5)
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

Probit_PS_pred <- function(data, 
                        target_var = "T",
                        vars_to_exclude = c("Y", "PS_scaled")) {
  drop_vars <- c(target_var, vars_to_exclude)
  regressors <- names(data)[!names(data) %in% drop_vars]
  f <- paste(target_var, "~", paste(regressors, collapse = "+"))
  probit_reg <- glm(f, data = data, family = binomial(link = "probit"))
  PS_pred_probit <- as.vector(predict.glm(object = probit_reg, newdata = data, type = "response"))
  return(PS_pred_probit)
}

True_PS_pred <- function(data){
  PS_true <- data$PS_scaled
  return(PS_true)
}

###
# Double/debiased ML
###
#Not directly used as PS estimator, but directly estimates treatment effect; hence
#later called in the "estimate_TE" function

DoubleML_treatment_est <- function(data) {
  #Prepare data
  dataset <- data[,names(data) != "PS_scaled"]
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
# Combining PS estimators 
###

PS_estimators <- function(data,
                          target_var = "T",
                          vars_to_exclude = c("Y", "PS_scaled"),
                          estimators = c("true", "log", "probit", "ridge", "lasso", "rf",
                                         "bayes_ridge", "bayes_lasso", "bayes_horseshoe", "bayes_horseshoePlus")) {
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
  if ("ridge" %in% estimators) {
    results <- append(results, list("ridge" = 
                                      Ridge_PS_pred(data, target_var, vars_to_exclude)))}
  if ("lasso" %in% estimators) {
    results <- append(results, list("lasso" = 
                                      Lasso_PS_pred(data, target_var, vars_to_exclude)))}
  if ("rf" %in% estimators) {
    results <- append(results, list("rf" = 
                                      suppressMessages(RF_PS_pred(data, target_var, vars_to_exclude))))} 
  if ("bayes_ridge" %in% estimators) {
    results <- append(results, list("bayes_ridge" = 
                                      Bayes_Ridge_PS_pred(data, target_var, vars_to_exclude)))}
  if ("bayes_lasso" %in% estimators) {
    results <- append(results, list("bayes_lasso" = 
                                      Bayes_Lasso_PS_pred(data, target_var, vars_to_exclude)))}
  if ("bayes_horseshoe" %in% estimators) {
    results <- append(results, list("bayes_horseshoe" = 
                                      Bayes_Horseshoe_PS_pred(data, target_var, vars_to_exclude)))}
  if ("bayes_horseshoePlus" %in% estimators) {
    results <- append(results, list("bayes_horseshoePlus" = 
                                      Bayes_HorseshoePlus_PS_pred(data, target_var, vars_to_exclude)))}
  return(results)
}

###
# Estimate TE
###
#This should estimate treatment effects given all combinations of matches
estimate_TE <- function(list_of_matches, data, include_DML = TRUE){
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
  #Add double ML result
  if(include_DML){
    double_ML_estimate <- DoubleML_treatment_est(data = data)[[1]]
    results$DML <- double_ML_estimate}
  return(results)
}

###
# Testing
###
# PS_scores <- PS_estimators(dataset,
#                            estimators = c("true", "log", "probit", "ridge", "lasso", "rf",
#                                           "bayes_ridge", "bayes_lasso", "bayes_horseshoe", "bayes_horseshoePlus"),
#                            target_var = "T",
#                            vars_to_exclude = c("Y", "PS_scaled"))
