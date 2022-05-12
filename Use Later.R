#Not really understood how to set the distribution up yet
xx<-rnorm(1000)
parms<-JohnsonFit(xx)
hist(rJohnson(n=1000, parms))

#Trials for the calculate TE function
lapply(matched_data[[1]], lm(Y~T, data = matched_data, weights = weights))
lapply(matched_data, function(x) lapply(x, run_regression, matched_data, weights))
lapply(matched_data, function(x) lapply(x, function(y) run_regression(matched_data = y$matched_data, weights = y$weights)))
lapply(matched_data, function(x) lapply(x, function(y) print(head(y$weights))))
lapply(test4, function(x) lapply(x, function(y) lm(Y~T, data = x$matched_data, weights = x$weights)))
lapply(test4, function(x) lapply(x, function(b) lm(f, data = b[[1]], weights = b[[2]])))
lapply(test4, function(x) lapply(x, function(y) print(y[[1]])))
lapply(test4, function(x) print(summary(x)))
lapply(test4, function(x) lapply(x, function(y) print(names(y[[1]]))))
lapply(matched_data, function(x) print(names(x)))
#Somehow, everything I tried with lapply stopped working when lm was used, printing data was fine with twp nested lapply functions


#Old implementation of monte carlo simulation
sim_1 <- function(n_obs, n_A, n_B, n_C) {
  dataset = genDSmultivariate_1(n_obs = n_obs,
                                n_A = n_A,
                                n_B = n_B,
                                n_C = n_C,
                                treatment_effect = 20,
                                v_range = c(-0.3, 0.3), 
                                alpha_PS = 5)
  X <- as.matrix(dataset[,!names(dataset) %in% c("Y", "T")])
  T <- as.matrix(dataset[,"T"])
  
  cv.lasso <- cv.glmnet(x = X, y = T, alpha = 1, family = "binomial")
  cv.ridge <- cv.glmnet(x = X, y = T, alpha = 0, family = "binomial") #wieso macht es hier einen Unterschied wenn ic nach binomial "family = "logit"" habe ?
  
  log_reg <- glm(T ~ . - Y, data = dataset, family = binomial(link = "logit"))
  log_lasso_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min) 
  log_ridge_reg <- glmnet(x = X, y = T, family = "binomial", alpha = 0, lambda = cv.ridge$lambda.min)
  
  PS_pred_log <- predict.glm(object = log_reg, newdata = dataset, type = "response")
  PS_pred_lasso <- predict(object = log_lasso_reg, newx = X, type = "response")
  PS_pred_ridge <- predict(object = log_ridge_reg, newx = X, type = "response")
  PS_pred_RF <- RF_PS_pred(data = dataset)
  
  match_log <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_log)))
  match_log_lasso <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_lasso)))
  match_log_ridge <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_ridge)))
  match_RF <- matchit(T~X, data = dataset, method = "nearest", distance = as.vector(unlist(PS_pred_RF)))
  
  md_log <- match.data(match_log)
  md_log_lasso <- match.data(match_log_lasso)
  md_log_ridge <- match.data(match_log_ridge)
  md_RF <- match.data(match_RF)
  
  md_log_weights <- md_log$weights
  md_log_lasso_weights <- md_log_lasso$lasso
  md_log_ridge_weights <- md_log_ridge$weights
  md_RF_weights <- md_RF$weights
  
  fit_log <- lm(Y~T, data = md_log, weights = md_log_weights)
  fit_lasso <- lm(Y~T, data = md_log_lasso, weights = md_log_lasso_weights)
  fit_ridge <- lm(Y~T, data = md_log_ridge, weights = md_log_ridge_weights)
  fit_RF <- lm(Y~T, data = md_RF, weights = md_RF_weights)
  
  return(list("Log" = fit_log$coefficients[["T1"]], 
              "Lasso" = fit_lasso$coefficients[["T1"]], 
              "Ridge" = fit_ridge$coefficients[["T1"]],
              "RF" = fit_RF$coefficients[["T1"]]))
}

mean_vs_median<-function(n,scale){
  
  # generate sample
  sample<-rnorm(n, 0, scale)
  
  # calculate estimators
  mean_sample<-mean(sample)
  median_sample<-median(sample)
  
  A = mvrnorm(n = 1000, mu = rep(0, 5), Sigma = diag(5))
  unlist(sfLapply(A, rbinom, n = 1, size = 1))
  #packages seem not to be the issue
  
  # return results
  return(list("mean"=mean_sample, "median"=median_sample))
}

n_grid<-c(50, 250, 500)
scale_grid<-c(1, 2, 4)

param_list=list("n"=n_grid, "scale"=scale_grid)

erg_mean_median <- MonteCarlo(func=mean_vs_median, nrep=10000, param_list=param_list, ncpus = 4)

MakeTable(output=erg_mean_median, rows="n", cols=c("scale", "list"), digits=4, include_meta=FALSE)

###
# Generate dataset with univariate X
###

genDSunivariateX_1 <- function (n_obs = 2000,
                                share_treated = 0.2,
                                X_dist_shift = 1, # Determines shift of X distribution in univariate case in treated population
                                alpha_PS = 1,
                                beta_PS = 0.6,
                                gamma_PS = 0, 
                                treatment_effect = 0.2) {
  #Generate X distribution and treatment indicator
  x_t <- rnorm(n_obs*share_treated, X_dist_shift, 1)
  x_nt<- rnorm(n_obs*(1-share_treated), 0, 1)
  
  #Generate binary vector for treatment assignment
  treat <- c(rep(1, n_obs*share_treated), rep(0,n_obs*(1-share_treated)))
  
  ps_t <- PS_fun(alpha = alpha_PS, beta = beta_PS, gamma = gamma_PS, X = x_t) #Is it appropriate to have differing parameters here?
  ps_nt<- PS_fun(alpha = 0, beta = beta_PS, gamma = gamma_PS, X = x_nt)
  
  ps_t_nt <- c(ps_t, ps_nt)
  ps_t_nt <- rescale(ps_t_nt, to = c(0,1))
  #Probably must be re-scaled using alpha and beta!?
  
  #Connect propensity score to outcome
  outcome_t <- ps_t_nt[1:(n_obs*share_treated)] * 0.7 + treatment_effect 
  outcome_nt<- ps_t_nt[(n_obs*share_treated+1):(length(ps_t_nt))]* 0.7 
  
  #Combine vectors to one dataset
  outcome_t_nt <- c(outcome_t, outcome_nt)
  x_t_nt <- c(x_t, x_nt)
  
  dataset <- data.frame(x_t_nt, ps_t_nt, outcome_t_nt, treat)
  colnames(dataset) <- c("X", "PS", "Y", "T")
  return(dataset)
}

#Own function created to create multivariate normal distributions
gen_cor_dist <- function(n_obs = 500, n_var = 5, min_covar = 0.2, max_covar = 4, digits = 1) {
  matrix = matrix(nrow = n_var, ncol = n_var)
  sigma_random <- t(sapply(1:nrow(matrix), 
                           function(i) 
                             c(rep(0,i-1), 
                               round(
                                 runif(n_var +1 -i, min = min_covar, max = max_covar),
                                 digits = digits)))) 
  X = mvrnorm(n = n_obs, mu = rep(0, n_var), Sigma = sigma_random)
  return(X)
}

gen_cor_dist <- function(n_obs = 500, n_var = 5, min_covar = 0.2, max_covar = 0.99, digits = 1) {
  matrix = matrix(nrow = n_var, ncol = n_var)
  sigma_random <- t(sapply(1:nrow(matrix),
                           function(i)
                             c(rep(0,i-1),
                               1,
                               round(
                                 runif(n_var-i, min = min_covar, max = max_covar),
                                 digits = digits))))
  X = mvrnorm(n = n_obs, mu = rep(0, n_var), Sigma = sigma_random)
  return(X)
}

#Dont really understand how it workds
#Covariance with custom degree of colinearity
# for (var in c(1, 1.1, 1.5, 2, 8, 20, 100, 1000)) {
# 
#   sigma = genPositiveDefMat(dim = n_X, rangeVar = c(0,var))$Sigma
#   X = mvrnorm(n = n_obs, mu = rep(0,n_X), Sigma = sigma)
#   print(paste("upper:", var))
#   print(mean(cor(X)))
# }

#Plot distribution of bias
#Probably needs some modification to show distribution of one particular set-up
#Should not be used
dist_bias_sim_result <- function(sim_result, params =c("n_obs", "DGPS")) {
  data <- MakeFrame(sim_result)
  data_long <- melt(setDT(data), id.vars = params, variable.name = "estimators")
  plot_estimate_dist <- ggplot(data_long, aes(x=value, color = estimators)) +
    #plot_estimate_dist <- ggplot(data_long[n_obs==500,], aes(x=value, color = estimators)) +
    geom_density()  
  plot_estimate_dist
}