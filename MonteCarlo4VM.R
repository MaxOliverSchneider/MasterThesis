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

sim_VM <- function(DGP, n_obs){
  PS_impact = "linear"
  Y_impact = "linear"
  if (DGP == "quadratic_linear") {PS_impact = "quadratic"}
  if (DGP == "quadratic2_linear") {PS_impact = "quadratic_2"}
  if (DGP == "quadratic3_linear") {PS_impact = "quadratic_3"}
  if (DGP == "linear_quadratic") {Y_impact = "quadratic"}
  if (DGP == "quadratic_quadratic") {
    Y_impact = "quadratic" 
    PS_impact = "quadratic"}

  
  dataset <- gen_DS_modular(
    PS_impact = PS_impact, Y_impact = Y_impact, n_obs = n_obs,
    X_dim = 10, X_impact_share_PS = 0.7, X_covar = 0.5, X_impact_share_Y = 0.7, 
    X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, adjust_alpha_Y = TRUE)
  #Check for excessive mean diff ins PS scores
  mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
  PS_est <- c("true", "log", "probit", "ridgeInterPoly", "lassoInterPoly", 
              "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
  Matching_estimators <- c("nn_one", "nnk", "stratum", "caliper_est", "cs")
  
  if (mean_diff >= 0.25) {
    combined_estimators <- c(apply(expand.grid(
      PS_est, replace(
        Matching_estimators, Matching_estimators == "caliper_est", "caliper"))[,c(2,1)], 1, base::paste0, collapse="_"), "DML")
    bias = setNames(as.list(rep(9999, length(combined_estimators))), combined_estimators)
  } else {
    
    PS_scores <- PS_estimators(dataset,
                               estimators = PS_est,
                               target_var = "T",
                               vars_to_exclude = c("Y", "PS"))
    matched_data <- matching_function(PS_scores = PS_scores,
                                      dataset = dataset,
                                      matching_estimators = Matching_estimators)
    TE_effects <- estimate_TE(matched_data, data = dataset, include_DML = TRUE, include_SimpleReg = TRUE)
    bias <- lapply(TE_effects, function(x) x - 1)
  }
  return(bias)
}

param_list <- list("n_obs" = c(1000,3000),
                   "DGP" = c( "quadratic_linear", "quadratic2_linear", "quadratic3_linear",
                              "linear_quadratic", "quadratic_quadratic"))

sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 25,
                                   param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
save(sim_VM_result, file = "sim_VM_21.Rdata")
