scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R", "Graphs.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

###
#Testing performance of the PS estimators
###
results = c()
for (i in c(1, 5, 30)) {
  #PS_impact = "interaction"
  PS_impact = "polynomial"
  dataset <- gen_DS_modular(PS_impact = PS_impact, X_dim = i)
  
  #Determine which estimators to use
  PS_est <- c("log","ridgeNaive", "ridgeInterPoly", 
              "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
  
  PS_scores <- PS_estimators(dataset,
                             estimators = PS_est,
                             target_var = "T",
                             vars_to_exclude = c("Y", "PS"))
  
  #Give share of correct classifications if PS>0.5 --> T = 1
  results = rbind(results,PS_predictive_performance(dataset, PS_scores))
  #Plot predicted vs. true PS scores
  output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
  do.call("grid.arrange", c(output, top = paste0("X_dim = ", i," PS_impact = ", PS_impact)))
}
#do.call("grid.arrange", output)

dataset <- gen_DS_modular(
  PS_impact = "kink", to_interact_PS = list(c(1,2),c(3,4),c(5,6)), Y_impact = "linear", n_obs = 1000,
  X_dim = 10, X_impact_share_PS = 0.7, X_covar = 0.5, X_impact_share_Y = 0.7, 
  X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.1, adjust_alpha_Y = TRUE,
  alpha_PS = 0.8)
plot_PS_density(dataset)


#Testing with interactions
results = c()
for (X_dim in c(10)) {
  dataset <- gen_DS_modular(
    PS_impact = "interaction_mult", to_interact_PS = list(c(1,2),c(3,4),c(5,6)), 
    Y_impact = "linear", n_obs = 15000,
    X_dim = X_dim, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7, 
    X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, 
    beta_adjust_power_PS = 0.1,
    adjust_alpha_Y = TRUE, alpha_PS = -0.2)
  # print(plot_PS_density(dataset))
  # Determine which estimators to use
  PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
              "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")

  PS_scores <- PS_estimators(dataset,
                             estimators = PS_est,
                             target_var = "T",
                             vars_to_exclude = c("Y", "PS"))

  #Give share of correct classifications if PS>0.5 --> T = 1
  results = rbind(results,PS_predictive_performance(dataset, PS_scores))
  #Plot predicted vs. true PS scores
  output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
  do.call("grid.arrange", output)
}

#Testing with share of X _vars without impact
for (i in c(10, 100, 1000)) {
  dataset <- gen_DS_modular(X_dim = i,
                            X_impact_share_PS = 0.5,
                            X_impact_share_Y = 0.6,
                            X_impact_shift_percent_Y = 0.1,
                            adjust_beta_PS = TRUE,
                            beta_adjust_power_PS = 0.5, 
                            n_obs = 500)
  # Determine which estimators to use
  PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
              "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")

  PS_scores <- PS_estimators(dataset,
                             estimators = PS_est,
                             target_var = "T",
                             vars_to_exclude = c("Y", "PS"))

  #Give share of correct classifications if PS>0.5 --> T = 1
  results = rbind(results,PS_predictive_performance(dataset, PS_scores))
  #Plot predicted vs. true PS scores
  output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
  do.call("grid.arrange", c(output, top = paste0("X_dim = ", i," PS_impact = ", PS_impact)))
}

dataset <- gen_DS_modular(n_obs = 1000, X_dim = 20, PS_impact = "range_assign", adjust_beta_PS = TRUE)
PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
            "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "rf2")
PS_scores <- PS_estimators(dataset,
                           estimators = PS_est,
                           target_var = "T",
                           vars_to_exclude = c("Y", "PS"))
PS_predictive_performance(dataset, PS_scores)
output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
do.call("grid.arrange", output)

#testing performance of tuned random forest depending on n_trees

n_trees <- c(300, 350, 400, 450, 500, 550, 600)
dataset <- gen_DS_modular(
  n_obs = 1500, X_covar = 0.3,
  X_dim = 10, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7, PS_impact = "range_assign",
  X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_PS = 0.4, alpha_PS = -0.2, beta_adjust_power_PS = 0.2)
results <- data.frame(matrix(ncol=4, nrow=1))

for (i in n_trees){
  print(i)
  PS_score <- RF_PS_pred(dataset, n_trees = i)
  data = data.frame(cbind(dataset$PS, PS_score, dataset$T))
  RMSE = round((mean((data[,1]-data[,2])^2))^0.5, digits = 2)
  MAE = round(mean(abs(data[,1] - data[,2])), digits = 2)
  COR = round(cor(data[,1], data[,2]), digits = 3)
  results <- rbind(results, c(i, RMSE, MAE, COR))
}

dataset <- gen_DS_modular(X_dim = 10, X_impact_share_PS = 0.7, PS_impact =  "quadratic", alpha_PS = -2.5)
PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
            "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")

PS_scores <- PS_estimators(dataset,
                           estimators = PS_est,
                           target_var = "T",
                           vars_to_exclude = c("Y", "PS"))

#Give share of correct classifications if PS>0.5 --> T = 1
results = rbind(results,PS_predictive_performance(dataset, PS_scores))
#Plot predicted vs. true PS scores
output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
do.call("grid.arrange", output)

###
# Evaluating output from testing PS estimators on VM
###

selector <- 3
result <- results[[selector]]
output = plot_PS_estimator_performance(dataset = result[[2]], PS_scores = result[[3]][-1])
png(file=paste0("C:/Users/max/Documents/Master/FS 21/MA/images/ConditionalInteractionsEasyThreshold_", selector,".png"),
    width=1200, height=700)
full_output <- do.call("grid.arrange", c(output, top = paste0("Dummy share = ", result[[1]])))
dev.off()
PS_predictive_performance(result[[2]], result[[3]])     


#Adding noise or bias to PS scores to see how it affects matching
dataset_keep <- dataset
PS_scors_keep <- PS_scores_base

bias = -0.3
PS_scores <- PS_scores_base
#Adding noise
PS_scores$noise <- PS_scores_base[[1]] + sample(
  c(bias, 0, -bias),size = length(PS_scores_base[[1]]), replace = TRUE)
PS_scores$noise <- ifelse(PS_scores$noise >1, 1, PS_scores$noise)
PS_scores$noise <- ifelse(PS_scores$noise <0, 0, PS_scores$noise)
PS_scores$partialShift <- PS_scores_base[[1]]
#Shifting half of the observations up, the other down
PS_scores$partialShift[1:500] <- PS_scores_base[[1]][1:500] + bias
PS_scores$partialShift[501:1000] <- PS_scores_base[[1]][501:1000] - bias
PS_scores$partialShift <- ifelse(PS_scores$partialShift >1, 1, PS_scores$partialShift)
PS_scores$partialShift <- ifelse(PS_scores$partialShift <0, 0, PS_scores$partialShift)
#Substituting half the PS scores with random numbers
PS_scores$halfRandom <- PS_scores_base[[1]]
PS_scores$halfRandom[1:500] <- runif(500)
PS_scores$allRandom <- runif(1000)


matched_data <- matching_function(PS_scores = PS_scores,
                                  dataset = dataset,
                                  matching_estimators = Matching_estimators)
TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
bias <- lapply(TE_effects, function(x) x - 1)


#Testing models from new paper
PS_impact = "quadraticSymmetric"
PS_link = "raw"
if (PS_impact %in% c("flat", "linear", "quadraticSymmetric", 
                     "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
dataset <- gen_DS_modular(Y_impact = "paper", PS_function = "paper", 
                          PS_impact = PS_impact, PS_link = PS_link, min_X = 0)
# head(dataset)
PS_est <- c("true","log")

PS_scores <- PS_estimators(dataset,
                           estimators = PS_est,
                           target_var = "T",
                           vars_to_exclude = c("Y", "PS"))
# matched_data <- matching_function(PS_scores = PS_scores,
#                                   dataset = dataset,
#                                   matching_estimators = Matching_estimators)
# TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE, PS_scores = PS_scores)
# head(PS_scores[[2]])
# #True effect is -1.67
# PS_est <- PS_scores[[1]][1:3]
# dataset <- dataset[1:3,]
#IPW
PS_est <- PS_scores[[1]]
Y = dataset$Y
T = as.numeric(dataset$T) - 1
divisor <- 1/length(T)
first_term <- (Y*T) / PS_est
second_term <- (Y*(1-T)) / (1-PS_est)
TE_IPW <- divisor * sum(first_term - second_term)

#NIPW
Y = dataset$Y
T = as.numeric(dataset$T) -1
first_term <- sum((Y*T)/PS_est) / sum(T/PS_est)
second_term <- sum((Y*(1-T))/(1-PS_est)) / sum((1-T)/(1-PS_est))
TE_NIPW <- first_term - second_term

#Difference in means estimator
size_treat <- sum(dataset$T==1)
size_Ntreat <- sum(dataset$T==0)
first_term <- sum((as.numeric(dataset$T)-1)*dataset$Y)
second_term <- sum(dataset$Y*(1-(as.numeric(dataset$T)-1)))
TE_MeanEst <- (1/size_treat)*first_term - (1/size_Ntreat)*second_term

TE_IPW
TE_NIPW
TE_MeanEst