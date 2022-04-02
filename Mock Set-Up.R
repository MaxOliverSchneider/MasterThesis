###
# Set-Up
###

#Loading required packages
packages <- c("stargazer", "MatchIt", "scales", "SuppDists", "lmtest",
              "sandwich", "DoubleML", "MASS", "caret", "glmnet", "tidymodels",
              "workflows", "tune", "tidyverse")
lapply(packages, require, character.only = TRUE)

#Loading functions from other scripts
scripts = c("GenerateData.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

###
#Simple set-up related to 2004 Frölich paper
###

dataset <- genDSunivariateX() 
dataset <- genDSunivariateX(share_treated = 0.2,
                            X_dist_shift = 1,
                            alpha_PS = 0,
                            gamma_PS = 0.2)
dataset = genDSmultivariate_1()
dataset = genDSmultivariate_1(treatment_effect = 20, n_A = 3, n_B = 2, n_C = 2, v_range = c(-0.3, 0.3), alpha_PS = 5)
X <- as.matrix(dataset[,!names(dataset) %in% c("Y", "T")])
T <- as.matrix(dataset[,"T"])

#Logistic regression
log_reg <- glm(T ~ . - Y, data = dataset, family = binomial(link = "logit"))
#stargazer(log_reg, type = "text")

#Logistic lasso regression, lambda chosen by cross-validation
cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = "binomial")
log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
#Could also use cv.lasso$lambda.1se which could give more simple but stable model

#Ridge regression, lambda chosen by cross-validation
cv.ridge <- cv.glmnet(x = X, y = T, alpha = 0, family = "binomial"(link = "logit"))
log_ridge_reg <- glmnet(x = X, y = T, family = "binomial"(link = "logit"), alpha = 0, lambda = cv.ridge$lambda.min)

#Predictions of PS
dataset["PS_pred_log"] <- predict.glm(object = log_reg, newdata = dataset, type = "response")
dataset["PS_pred_lasso"] <- predict(object = log_lasso_reg, newx = X, type = "response")
dataset["PS_pred_ridge"] <- predict(object = log_ridge_reg, newx = X, type = "response")

#Create predictions of T from predicted PS
dataset["T_pred_log"] = dataset["PS_pred_log"] > 0.5 
dataset["T_pred_log"] <- as.factor(as.numeric(dataset[["T_pred_log"]]))
dataset["T"] <- as.factor(dataset[["T"]])
dataset %>% 
  conf_mat(truth = T, estimate = T_pred_log)

dataset%>%
  ggplot() +
  geom_density(aes(x = PS_pred_log, fill = T), 
               alpha = 0.5)

###
# Matching (https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#k1-matching-ratio)
###

# One-on-one, no replacement, logit-PS
match_test_2 <- matchit(T ~ . - Y, data = dataset,
                        method = "nearest", 
                        distance = "glm")
match_test_2
summary(match_test_2)
plot(match_test_2, type = "jitter", interactive = FALSE)
plot(summary(match_test_2))


###
# Computing treatment effects (depends on used matching method)
###

#For 1:1 matching without replacement
md_1 <- match.data(match_test_2)
fit1 <- lm(Y~T, data = md_1, weights = weights)
coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
#Could also attain standard errors using bootstrapping as shown in https://cran.r-project.org/web/packages/MatchIt/vignettes/estimating-effects.html
#Can also include covariates
fit2 <- lm(Y~T + X + T*X, data = md_1, weights = weights)
coeftest(fit2, vcov. = vcovCL, cluster = ~subclass)#["T",, drop = FALSE] # 'Can't interpret the other coefficients causally

match_test_3 <- matchit(T~X, data = dataset,
                        method = "nearest", 
                        distance = as.vector(unlist(dataset["PS_pred_ridge"])))


###
# Testing double/debiased ML
###

###
# Calculate PS using RF (https://www.rebeccabarter.com/blog/2020-03-25_machine_learning/)
### 

#Creating model
rf <- rand_forest(mode = "classification",
                  engine = "ranger",
                  mtry = tune(),
                  trees = tune(),
                  min_n = tune()) %>%
  set_engine("ranger", importance = "impurity")

#Specify parameter grid
rf_grid <- expand.grid(mtry = c(3,4,5),
                       trees = c(10, 20, 50),
                       min_n = c(5, 10, 20))

#Splitting dataset into train/test (not really required in this case!?)
data_split <- initial_split(dataset, prop = 3/4)
dataset_train <- training(data_split)
dataset_test <- testing(data_split)
dataset_cv <- vfold_cv(dataset_train)

#Define recipe (which includes pre-processing and the regression formula)
recipe <- recipe(dataset[,!names(dataset) %in% c("Y")]) %>%
  update_role(T, new_role = "outcome") %>%
  update_role(-T, new_role = "predictor") %>%
  step_mutate(T = as.factor(T))

#Combine in a workflow
rf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf)

#Tune Hyperparameters
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = dataset_cv,
            grid = rf_grid, 
            metric = metric_set(accuracy, roc_auc))

#Check results
rf_tune_results %>% collect_metrics()

#Extract best parameters
param_final <- rf_tune_results %>% 
  select_best(metric = "accuracy")
param_final

rf_workflow <- rf_workflow %>% finalize_workflow(param_final)

#Fit on training, evaluate on test set
rf_fit <- rf_workflow %>% 
  last_fit(data_split)

test_performance <- rf_fit %>% collect_metrics()

# generate predictions from the test set
test_predictions <- rf_fit %>% collect_predictions()
test_predictions

# generate a confusion matrix
test_predictions %>% 
  conf_mat(truth = T, estimate = .pred_class)

#plot distributions of the predicted probability distributions for each class
test_predictions%>%
  ggplot() +
  geom_density(aes(x = .pred_1, fill = T), 
               alpha = 0.5)

#Fit final model
final_model <- fit(rf_workflow, dataset)
final_model

#Predict (on existing data)
predict(final_model, new_data = dataset)

#Extract variable importance
ranger_obj <- extract_fit_parsnip(final_model)$fit
sort(ranger_obj$variable.importance, decreasing = TRUE)
