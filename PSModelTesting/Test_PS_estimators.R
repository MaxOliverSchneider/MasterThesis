#Might imporve error logging on VM
options(keep.source = TRUE)

#Load packages
packages <- c("MonteCarlo", "snowfall", "tryCatchLog", "foreach", "doParallel")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

# data_sizes <- c(500, 1500, 10000)
# 
# results <- vector("list", length = 2)
# for (i in 1:3) {
#   dataset <- gen_DS_modular(
#     PS_impact = "interaction_mult", to_interact_PS = list(c(1,2),c(3,4),c(5,6)),
#     Y_impact = "linear", n_obs = data_sizes[i], X_factor_share = 1,
#     X_dim = 10, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7,
#     X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.1,
#     adjust_alpha_Y = TRUE, alpha_PS = -0.2)
# 
#   # Determine which estimators to use
#   PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
#               "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly",
#               "bayes_lassoInterPoly", "rf", "nn")
# 
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   result <- list()
#   result[[1]] <- data_sizes[i]
#   result[[2]] <- dataset
#   result[[3]] <- PS_scores
#   results[[i]] <- result
# }
# 
# save(results, file = "PS_estimator_results/Interaction_WithFactors.Rdata")

# X_dim <- c(10, 50, 100)
# 
# results <- vector("list", length = 3)
# for (i in 1:3) {
#   normalize = 0.1
#   if (i != 1) {normalize = 0.4}
#   dataset <- gen_DS_modular(
#     PS_impact = "linear",# to_interact_PS = list(c(1,2),c(3,4),c(5,6)),
#     Y_impact = "linear", n_obs = 500,
#     X_dim = X_dim[i], X_impact_share_PS = 0.2, X_impact_share_Y = 0.2,
#     X_impact_shift_percent_Y = 0.1, adjust_beta_PS = TRUE, beta_adjust_power_PS = normalize,
#     adjust_alpha_Y = TRUE, alpha_PS = -0.2)
# 
#   # Determine which estimators to use
#   PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
#               "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly",
#               "bayes_lassoInterPoly", "rf", "nn")
# 
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   result <- list()
#   result[[1]] <- X_dim[i]
#   result[[2]] <- dataset
#   result[[3]] <- PS_scores
#   results[[i]] <- result
# }
# 
# save(results, file = "PS_estimator_results/HighDim.Rdata")

# covars <- c(0.1, 0.3, 0.6, 0.9)
# 
# results <- vector("list", length = 4)
# for (i in 1:4) {
  # dataset <- gen_DS_modular(
  #   PS_impact = "linear",# to_interact_PS = list(c(1,2),c(3,4),c(5,6)),
  #   Y_impact = "linear", n_obs = 500, X_covar = covars[i],
  #   X_dim = 10, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7,
  #   X_impact_shift_percent_Y = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.4,
  #   adjust_alpha_Y = TRUE, alpha_PS = -0.2)
# 
#   # Determine which estimators to use
#   PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
#               "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly",
#               "bayes_lassoInterPoly", "rf", "nn")
# 
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   result <- list()
#   result[[1]] <- covars[i]
#   result[[2]] <- dataset
#   result[[3]] <- PS_scores
#   results[[i]] <- result
# }
# 
# save(results, file = "PS_estimator_results/Covar.Rdata")
# 
# dummy_share <- c(0, 0.3, 0.7, 1)
# 
# results <- vector("list", length = 4)
# for (i in 1:4) {
#   if (i == 1) {alpha_PS = -0.5}
#   if (i == 2) {alpha_PS = -1}
#   if (i == 3 | i == 4) {alpha_PS = -1.9}
#   dataset <- gen_DS_modular(
#     Y_impact = "linear", n_obs = 500, X_dummy_share = dummy_share[i],
#     X_dim = 10, X_impact_share_PS = 0.7, X_impact_share_Y = 0.7,
#     X_impact_shift_percent_Y = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.4,
#     adjust_alpha_Y = TRUE, alpha_PS = alpha_PS)
#   print(head(dataset))
#   #Determine which estimators to use
#   PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
#               "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly",
#               "bayes_lassoInterPoly", "rf", "nn")
# 
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   result <- list()
#   result[[1]] <- dummy_share[i]
#   result[[2]] <- dataset
#   result[[3]] <- PS_scores
#   results[[i]] <- result
# }
# 
# save(results, file = "PS_estimator_results/Dummy.Rdata")
# 
# results <- vector("list", length = 3)
# for (i in 1:3) {
#   if (i==1){
#   dataset <- gen_DS_modular(n_obs = 2000, X_dim = 10, 
#                  PS_impact = "conditional_interact_1",
#                  to_interact = list(c(1,2),c(3,4),c(5,6)), 
#                  X_impact_share_PS = 0.7,
#                  adjust_beta_PS = TRUE,
#                  beta_adjust_power_PS = 0.1,
#                  beta_PS = 1.25
#   )}
#   if (i==2){
#   gen_DS_modular(n_obs = 2000, X_dim = 10, 
#                  PS_impact = "conditional_interact_2",
#                  to_interact = list(c(1,2),c(3,4),c(5,6)), 
#                  X_impact_share_PS = 0.7,
#                  adjust_beta_PS = TRUE,
#                  beta_adjust_power_PS = 0.3,
#                  beta_PS = 1.25
#   )}
#   if(i==3){gen_DS_modular(n_obs = 2000, X_dim = 10, 
#                  PS_impact = "conditional_interact_3",
#                  to_interact = list(c(1,2,3),c(4,5,6)), 
#                  X_impact_share_PS = 0.7,
#                  adjust_beta_PS = TRUE,
#                  beta_adjust_power_PS = 0.2,
#                  beta_PS = 2.6
#   )}
#   PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
#               "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly",
#               "bayes_lassoInterPoly", "rf", "nn")
#   
#   PS_scores <- PS_estimators(dataset,
#                              estimators = PS_est,
#                              target_var = "T",
#                              vars_to_exclude = c("Y", "PS"))
#   result <- list()
#   result[[1]] <- i
#   result[[2]] <- dataset
#   result[[3]] <- PS_scores
#   results[[i]] <- result
# }
# 
# save(results, file = "PS_estimator_results/ConditionalInteractions_EasierThreshhold.Rdata")
X_dim <- c(500,750)
results <- vector("list", length = 3)

for (i in c(2)) {
  beta_PS =0.4
  #if(i== 2) {beta_PS =0.35}
  #  if(i == 3){beta_PS=0.25}
    dataset <- gen_DS_modular(X_impact_share_within = 0.01, 
                              X_dim = X_dim[i], 
                              n_obs = 500,
                              PS_impact = "small_share",
                              beta_PS = beta_PS,
                              alpha_PS = -0.2)
  
  PS_est <- c("log","ridgeNaive", #"ridgeInterPoly", "lassoInterPoly", "bayes_ridgeInterPoly",
              #"bayes_lassoInterPoly",
              "lassoNaive",  "rf", "rf2")#, "nn")
  
  PS_scores <- PS_estimators(dataset,
                             estimators = PS_est,
                             target_var = "T",
                             vars_to_exclude = c("Y", "PS"))
  result <- list()
  result[[1]] <- i
  result[[2]] <- dataset
  result[[3]] <- PS_scores
  results[[i]] <- result
}

save(results, file = "PS_estimator_results/SmallShare.Rdata")