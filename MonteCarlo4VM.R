#setwd("C:/Users/max/Documents/Master/FS 21/MA/Fr?lich/Code/MA")

#Might imporve error logging on VM
options(keep.source = TRUE)

#Load packages
packages <- c("MonteCarlo", "snowfall", "tryCatchLog")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

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


sim_VM <- function(PS_impact, n_obs){
  PS_link = "raw"
  if (PS_impact %in% c("flat", "linear", "quadraticSymmetric",
                       "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
  dataset <- gen_DS_modular(n_obs = n_obs, Y_impact = "paper", PS_function = "paper",
                            PS_impact = PS_impact, PS_link = PS_link, min_X = 0)
  polynomials <- c(0,1,2,3,4,5,6,7,8,9)
  polynomials_vec <- c()
  PS_scores <- list()
  PS_models <- list()
  #Now checking how it behaves when trimming on true PS
  quantiles <- quantile(dataset$PS, c(0.02,0.98))
  if (PS_impact != "flat"){
  dataset <- dataset[dataset$PS>quantiles[[1]] & dataset$PS<quantiles[[2]],]}
  
  for (i in polynomials){
    polynomials_vec <- c(polynomials_vec, i)
    PS <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec)
    PS_scores <- append(PS_scores, list(PS))

    PS_model <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec, return_Model = TRUE)
    PS_models <- append(PS_models, list(PS_model))
  }
  names(PS_scores) <- polynomials
  names(PS_models) <- polynomials

  #Trimming and calculating treatment effects in same loop
  #datasets <- list()
  IPW_results <- list()
  NIPW_results <- list()
  IPW_VAR_results <-list()
  NIPW_VAR_results <-list()
  IPW_coverage <- list()
  NIPW_coverage <- list()


  for (i in polynomials+1){
    #indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
    # if(i>1){ #Because trimming is not working with a constant PS score
    #   datasets[[i]] <- dataset[indicator,]
    #   PS_scores[[i]] <- PS_scores[[i]][indicator]
    # } else {
    #   datasets[[i]] <- dataset
    #   PS_scores[[i]] <- PS_scores[[i]]
    #}
    IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  dataset)
    NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = dataset)

    #Get variance
    IPW_VAR_results[[i]] <-  est_var_w_polynomials(
      dataset = dataset, PS_score = PS_scores[[i]], TE = IPW_results[[i]], poly_degree = i-1)
    NIPW_VAR_results[[i]] <-  est_var_w_polynomials(
      dataset = dataset, PS_score = PS_scores[[i]], TE = NIPW_results[[i]], poly_degree = i-1)

    #Get coverage ration
    CI <- est_confidence_interval(
      IPW_VAR_results[[i]], TE = IPW_results[[i]], N = nrow(dataset), confidence_level = 0.975)
    IPW_coverage[[i]] <- (CI[1]<=IPW_results[[i]] & CI[2] >= IPW_results[[i]])
    CI <- est_confidence_interval(
      NIPW_VAR_results[[i]], TE = NIPW_results[[i]], N = nrow(dataset), confidence_level = 0.975)
    NIPW_coverage[[i]] <- (CI[1]<=NIPW_results[[i]] & CI[2] >= NIPW_results[[i]])
  }

  #MISE & SUP
  # MISEs <- lapply(PS_models, est_MISE, PS_impact = PS_impact, PS_link = PS_link, cols =1)
  # SUPs <- lapply(PS_models, est_SUP, PS_impact = PS_impact, PS_link = PS_link)

  names(IPW_results) <- paste0("IPW_w_poly", polynomials)
  names(NIPW_results) <- paste0("NIPW_w_poly", polynomials)
  names(IPW_VAR_results) <- paste0("IPW_VAR_w_poly", polynomials)
  names(NIPW_coverage) <- paste0("NIPW_coverage_w_poly", polynomials)
  names(IPW_coverage) <- paste0("IPW_coverage_w_poly", polynomials)
  names(NIPW_VAR_results) <- paste0("NIPW_VAR_w_poly", polynomials)
  # names(MISEs) <- paste0("MISE_w_poly_", polynomials)
  # names(SUPs) <- paste0("SUP_w_poly_", polynomials)
  # estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, NIPW_VAR_results,
  #                IPW_VAR_results, IPW_coverage, NIPW_coverage)
  estimates <- c(IPW_results, NIPW_results, NIPW_VAR_results,
                 IPW_VAR_results, IPW_coverage, NIPW_coverage)
   return(estimates)
}


param_list <- list("n_obs" = c(1000),
                   "PS_impact" = c("flat", "linear", "quadraticSymmetric",
                                   "quadraticNonSymmetric", "fourthDegree",
                                   "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
                                   "stepNonMonotonic"))


sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1500,
                                   param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
save(sim_VM_result, file = "sim_VM_48.Rdata")


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
# 
#   polynomials <- c(0,1,2,3,4,5,6,7,8,9)
#   polynomials_vec <- c()
#   PS_scores <- list()
#   PS_models <- list()
#   for (i in polynomials){
#     polynomials_vec <- c(polynomials_vec, i)
#     output <- Lasso_PS_pred(data = dataset, polynomials_vector = polynomials_vec, return_modelAndPS = TRUE, include_interactions = FALSE)
#     PS_scores <- append(PS_scores, list(output[[1]]))
#     PS_models <- append(PS_models, list(output[[2]]))
#   }
#     names(PS_scores) <- polynomials
#     names(PS_models) <- polynomials
# 
#     #Trimming and calculating treatment effects in same loop
#     datasets <- list()
#     IPW_results <- list()
#     NIPW_results <- list()
#     IPW_VAR_results <-list()
#     NIPW_VAR_results <-list()
#     IPW_coverage <- list()
#     NIPW_coverage <- list()
#     PS_constant <- list()
# 
#     # if(Sys.info()['sysname'] == "Windows") {quantiles_list <- lapply(PS_scores, quantile, c(0.02,0.98))
#     # } else{quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))}
#     quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#     #Trimming
#     for (i in polynomials+1){
#       indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
#       if(mean(indicator) > 0.95){ #If PS prediction is constat, indicator will throw out all observation
#         datasets[[i]] <- dataset[indicator,]
#         PS_scores[[i]] <- PS_scores[[i]][indicator]
#         PS_constant[[i]] <- 0
#       } else {
#         datasets[[i]] <- dataset
#         PS_scores[[i]] <- PS_scores[[i]]
#         PS_constant[[i]] <- 1
#       }
#       IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  datasets[[i]])
#       NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = datasets[[i]])
#     }
#   names(PS_constant) = polynomials
#   names(IPW_results) = polynomials
#   names(NIPW_results) = polynomials
# 
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                   PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS,
#                   alpha = alpha_PS, cols = "automatic", model_type = "lasso/ridge", impact_share = X_impact_share_PS)
#   SUPs <- lapply(PS_models, est_SUP, PSfun = "paper", beta_adjust_power = beta_adjust_power_PS, beta = beta_PS,
#                  PS_impact = PS_impact, PS_link = PS_link, alpha_adjust_power = alpha_adjust_power_PS,
#                  alpha = alpha_PS, cols = "automatic", model_type = "lasso/ridge", impact_share = X_impact_share_PS)
# 
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   # names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   # names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   # names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   # names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   names(MISEs) <- paste0("MISE_", names(MISEs))
#   names(SUPs) <- paste0("SUP_", names(SUPs))
#   names(PS_constant) <- paste0("ConstantPS_", names(PS_constant))
#   estimates <- c(IPW_results, NIPW_results, MISEs, SUPs, PS_constant)#, NIPW_VAR_results,
#   #IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(500),
#                    "X_dim" = c(300,700,1000),
#                    "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
# #                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 200,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_47.Rdata")

