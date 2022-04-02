sfLibrary("MASS")
#sfSource()
sfExportAll()

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
                                return_Matrix = FALSE
) {
  #Create data
  A = mvrnorm(n = n_obs, mu = rep(0, n_A), Sigma = diag(n_A))
  B = mvrnorm(n = n_obs, mu = rep(0, n_B), Sigma = diag(n_B))
  C = mvrnorm(n = n_obs, mu = rep(0, n_C), Sigma = diag(n_C))
  v = runif(n_obs, min = -2, max = 2)
  e = runif(n_obs, min = -2, max = 2)
  dataset = data.frame(A, B, C)
  colnames(dataset) = c(paste0("A", 1:n_A), paste0("B", 1:n_B), paste0("C", 1:n_C))
  
  #Calculate PS and treatment indicator
  PS_unscaled = rowSums(alpha_PS * A) + rowSums(beta_PS * B) + rowSums(gamma_PS * C) + v
  PS_scaled = rescale(PS_unscaled, to = c(0,1))
  dataset["T"] <- unlist(lapply(PS_scaled, rbinom, n = 1, size = 1))
  
  #Calculate outcome
  dataset["Y"] = treatment_effect * dataset["T"] + rowSums(alpha_outcome * A) + 
    rowSums(beta_outcome * B) + rowSums(gamma_outcome * C) + e
  if (return_Matrix) {return(as.matrix(dataset))} else {return(dataset)}
}

sim_1 <- function(n_obs, n_A, n_B, n_C) {
  n_obs = 1000
  treatment_effect = 10
  alpha_PS = 2
  beta_PS = 1
  gamma_PS = 0.1
  alpha_outcome = 2
  beta_outcome = 1
  gamma_outcome = 0.5
  n_A = 10
  n_B = 30
  n_C = 50
  return_Matrix = FALSE
  A = mvrnorm(n = n_obs, mu = rep(0, n_A), Sigma = diag(n_A))
  B = mvrnorm(n = n_obs, mu = rep(0, n_B), Sigma = diag(n_B))
  C = mvrnorm(n = n_obs, mu = rep(0, n_C), Sigma = diag(n_C))
  v = runif(n_obs, min = -2, max = 2)
  e = runif(n_obs, min = -2, max = 2)
  dataset = data.frame(A, B, C)
  colnames(dataset) = c(paste0("A", 1:n_A), paste0("B", 1:n_B), paste0("C", 1:n_C))
  
  #Calculate PS and treatment indicator
  PS_unscaled = rowSums(alpha_PS * A) + rowSums(beta_PS * B) + rowSums(gamma_PS * C) + v
  PS_scaled = rescale(PS_unscaled, to = c(0,1))
  dataset["T"] <- rbinom(n_obs, 1, 0.5)
  #dataset["T"] <- unlist(lapply(PS_scaled, rbinom, n = 1, size = 1))
  #Calculate outcome

  dataset["Y"] = treatment_effect * dataset["T"] + rowSums(alpha_outcome * A) +
    rowSums(beta_outcome * B) + rowSums(gamma_outcome * C) + e
  X <- as.matrix(dataset[,!names(dataset) %in% c("Y", "T")])
  T <- as.matrix(dataset[,"T"])

  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = "binomial")
  cv.ridge <- cv.glmnet(x = X, y = T, alpha = 0, family = "binomial"(link = "logit"))

  log_reg <- glm(T ~ . - Y, data = dataset, family = binomial(link = "logit"))
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
  log_ridge_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 0, lambda = cv.ridge$lambda.min)

  PS_pred_log <- predict.glm(object = log_reg, newdata = dataset, type = "response")
  PS_pred_lasso <- predict(object = log_lasso_reg, newx = X, type = "response")
  PS_pred_ridge <- predict(object = log_ridge_reg, newx = X, type = "response")

  match_log <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_log)))
  match_log_lasso <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_lasso)))
  match_log_ridge <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_ridge)))

  md_log <- match.data(match_log)
  md_log_lasso <- match.data(match_log_lasso)
  md_log_ridge <- match.data(match_log_ridge)

  md_log_weights = md_log$weights
  md_log_lasso_weights = md_log_lasso$weights
  md_log_ridge_weights = md_log_ridge$weights
  
  fit_log <- lm(Y~T, data = md_log, weights = md_log_weights)
  fit_lasso <- lm(Y~T, data = md_log_lasso, weights = md_log_lasso_weights)
  fit_ridge <- lm(Y~T, data = md_log_ridge, weights = md_log_ridge_weights)
  return(list("Log" = fit_log$coefficients[["T"]],
              "Lasso" = fit_lasso$coefficients[["T"]],
              "Ridge" = fit_ridge$coefficients[["T"]]))
  #return(list("test" = 2))
}

n_obs = c(500, 1000, 2000)
n_A = c(10)
n_B = c(20)
n_C = c(50, 100)

param_list = list("n_obs" = n_obs, "n_A" = n_A, "n_B" = n_B, "n_C" = n_C)

#sim_1_result <- MonteCarlo(func = sim_1, nrep = 1000, param_list = param_list)#, ncpus = 4)
#sim_2_result <- MonteCarlo(func = sim_1, nrep = 10, param_list = param_list)#, ncpus = 4)
sim_3_result <- MonteCarlo(func = sim_1, nrep = 10, param_list = param_list, ncpus = 4)
?MonteCarlo

###
# Learnings parallel computation
###
# 
# Commands (likely not needed)
#  - sfInit
#  - sfLibrary
#  - sfSource
# 
# Things that caused issues:
#   - lapply was not working properly
#   - all variables need to be declared explicitly, somehow seems like it is not so good at "passing" them
#   --> save all variables that need to be used later somewehere
#   - Loading functions form other scripts is not working that well, have not found a solution yet, except copying the function
#     explicitly into the function used in monte carlo simulation (might try sfSource)
#     --> Seems like the loading was working fine, error just pointed to the loaded function
#       as source of error. Loading was not the problem, but lapply inside the loaded function
#       as well as not properly saved variables inside it were the issues
#
# Nussubg weights error means that a variable/object that is needed is not declared
# For checking for errors: Clear environment and first run "normally" to make sure it is not just using some values/variables
#   that were defined in previous runs