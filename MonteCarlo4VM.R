#setwd("C:/Users/max/Documents/Master/FS 21/MA/Fr?lich/Code/MA")

#Might imporve error logging on VM
options(keep.source = TRUE)

set.seed(111)

#Set it so that e-0x is not used
options(scipen=999)

#Load packages
packages <- c("MonteCarlo", "snowfall", "tryCatchLog", "hdm")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R", "Metrics.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))



# sim_VM <- function(PS_formula, n_obs){
#   PS_link = "raw"
#   if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   X_dim = 1
#   dataset <- gen_DS_modular(n_obs = n_obs, 
#                             PS_formula= PS_formula, 
#                             PS_link = PS_link, 
#                             X_dim = X_dim, 
#                             X_impact_share_PS = 1,
#                             X_impact_shift_PS = 0,
#                             X_impact_share_Y = 1,
#                             X_impact_shift_Y = 0)
#   polynomials <- c(0,1,2,3,4,5,6,7,8,9)
#   polynomials_vec <- c()
#   PS_scores <- list()
#   PS_models <- list()
#   #Now checking how it behaves when trimming on true PS
#   #quantiles <- quantile(dataset$PS, c(0.02,0.98))
# 
#   for (i in polynomials){
#     polynomials_vec <- c(polynomials_vec, i)
# 
    # output <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec, return_Model = TRUE)
# 
#     PS_scores[[i+1]] <- output[[1]]
#     PS_models[[i+1]] <- output[[2]]
#   }
#   names(PS_scores) <- polynomials
#   names(PS_models) <- polynomials
# 
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
#   IPW_VAR_results <-list()
#   NIPW_VAR_results <-list()
#   IPW_coverage <- list()
#   NIPW_coverage <- list()
#   # Kernel_002 <- list()
#   # Kernel_010 <- list()
#   # Kernel_025 <- list()
#   # Kernel_050 <- list()
# 
# 
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
# 
#   for (i in polynomials+1){
# 
#     indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
# 
#     ###
#     #ATTENTION
#     ####
# 
#     #Get indicator from true PS
#     # if (PS_formula != "flat"){
#     # quantiles =  stats::quantile(dataset$PS, c(0.02,0.98))
#     # indicator = dataset$PS>quantiles[[1]] & dataset$PS<quantiles[[2]]} else {indicator = rep(TRUE, nrow(dataset))}
# 
#     ####
#     ####
#     ###
# 
#     #Enforce overlap
#     #indicator <- enforce_overlap(dataset, PS_scores[[i]])
# 
# 
#     if(i>1){ #Because trimming is not working with a constant PS score
#       datasets[[i]] <- dataset[indicator,]
#       PS_scores[[i]] <- PS_scores[[i]][indicator]
#     } else {
#       datasets[[i]] <- dataset
#       PS_scores[[i]] <- PS_scores[[i]]
#     }
#     IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  datasets[[i]])
#     NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = datasets[[i]])
#     # Kernel_002[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#     #                               bandwidth = 0.02)
#     # Kernel_010[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#     #                               bandwidth = 0.10)
#     # Kernel_025[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#     #                               bandwidth = 0.25)
#     # Kernel_050[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#                                   # bandwidth = 0.50)
# 
#   }
# 
#   #MISE & SUP
#   MISEs <- lapply(PS_models, est_MISE, PS_formula = PS_formula, PS_link = PS_link, cols =1)
#   SUPs <- lapply(PS_models, est_SUP, PS_formula = PS_formula, PS_link = PS_link, cols =1)
# 
# 
#   names(IPW_results) <- paste0("IPW_w_poly", polynomials)
#   names(NIPW_results) <- paste0("NIPW_w_poly", polynomials)
#   # names(Kernel_002) <- paste0("Kernel_bw002_w_poly", polynomials)
#   # names(Kernel_010) <- paste0("Kernel_bw010_w_poly", polynomials)
#   # names(Kernel_025) <- paste0("Kernel_bw025_w_poly", polynomials)
#   # names(Kernel_050) <- paste0("Kernel_bw050_w_poly", polynomials)
#   # names(IPW_MSEover1k) <- paste0("IPW_w_poly_exceed", polynomials)
#   # names(NIPW_MSEover1k) <- paste0("NIPW_w_poly_exceed", polynomials)
#   names(MISEs) <- paste0("MISE_w_poly_", polynomials)
#   names(SUPs) <- paste0("SUP_w_poly_", polynomials)
# 
#   estimates <- c(IPW_results, NIPW_results,
#   #               Kernel_002, Kernel_010, Kernel_025, Kernel_050,
#                  MISEs, SUPs)
# 
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "PS_formula" = c("flat", "linear", "quadraticSymmetric",
#                                    "quadraticNonSymmetric", "fourthDegree",
#                                    "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                                    "stepNonMonotonic"))
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
# # param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1500,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_58.Rdata")

# sim_VM <- function(PSFormula, XImpact, cor){
#   n_obs = 1000
#   PS_link = "raw"
#   if (PSFormula %in% c("flat", "linear", "quadraticSymmetric",
#                         "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   X_dim = 3
#   if(cor == "correlated"){X_dim = "correlated"}
# 
#   if(XImpact == "full") {
#     X_impact_share_PS = 1
#     X_impact_shift_PS = 0
#     X_impact_share_Y = 1
#     X_impact_shift_Y = 0
#   } else if(XImpact == "partial") {
#     X_impact_share_PS = 2/3
#     X_impact_shift_PS = 0
#     X_impact_share_Y = 2/3
#     X_impact_shift_Y = 1/3
#   }
#   dataset <- gen_DS_modular(n_obs = n_obs,
#                             PS_formula = PSFormula,
#                             PS_link = PS_link,
#                             X_dim = X_dim,
#                             X_impact_share_PS = X_impact_share_PS,
#                             X_impact_shift_PS = X_impact_shift_PS,
#                             X_impact_share_Y = X_impact_share_Y,
#                             X_impact_shift_Y = X_impact_shift_Y,
#                             adjust_alpha_Y = 1,
#                             adjust_standard_deviation_Y = 0.5)
# 
#   PS_scores <- list()
#   PS_models <- list()
# 
# 
#   for (y in c(0,6)) {
#     if (y == 0) {vars_to_exclude = c("Y", "PS")} else {vars_to_exclude = c("Y", "PS", "X3")}
#     out <- Log_PS_pred(data = dataset,
#                        polynomials_vector = c(0),
#                        return_Model = TRUE,
#                        vars_to_exclude = vars_to_exclude)
#     PS_scores[[1+y]] <- out[[1]]
#     PS_models[[1+y]] <- out[[2]]
#     out <- Log_PS_pred(data = dataset,
#                        return_Model = TRUE,
#                        vars_to_exclude = vars_to_exclude)
#     PS_scores[[2+y]] <- out[[1]]
#     PS_models[[2+y]] <- out[[2]]
#     out <- Log_PS_pred(data = dataset,
#                        interaction_terms = c(1),
#                        return_Model = TRUE,
#                        vars_to_exclude = vars_to_exclude)
#     PS_scores[[3+y]] <- out[[1]]
#     PS_models[[3+y]] <- out[[2]]
#     out <- Log_PS_pred(data = dataset,
#                        interaction_terms = c(1),
#                        polynomials_vector = c(1,2),
#                        return_Model = TRUE,
#                        vars_to_exclude = vars_to_exclude)
#     PS_scores[[4+y]] <- out[[1]]
#     PS_models[[4+y]] <- out[[2]]
#     out <- Log_PS_pred(data = dataset,
#                        interaction_terms = c(1,2),
#                        polynomials_vector = c(1,2),
#                        return_Model = TRUE,
#                        vars_to_exclude = vars_to_exclude)
#     PS_scores[[5+y]] <- out[[1]]
#     PS_models[[5+y]] <- out[[2]]
#     out <- Log_PS_pred(data = dataset,
#                        interaction_terms = c(1,2),
#                        polynomials_vector = c(1,2,3),
#                        return_Model = TRUE,
#                        vars_to_exclude = vars_to_exclude)
#     PS_scores[[6+y]] <- out[[1]]
#     PS_models[[6+y]] <- out[[2]]
#   }
# 
#   #Trimming and calculating treatment effects in same loop
#   datasets <- list()
#   IPW_results <- list()
#   NIPW_results <- list()
# 
# 
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
# 
#   for (i in (1:length(PS_scores))){
# 
#     indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
# 
#     ###
#     #ATTENTION
#     ####
# 
#     # #Get indicator from true PS
#     # if (PS_formula != "flat"){
#     #   indicator <- stats::quantile(dataset$PS, c(0.02,0.98))} else {indicator = rep(TRUE, nrow(dataset))}
#     #
#     # ####
#     # ####
#     # ###
#     if (!i %in% c(1,7)) { #Dont do it if PS scores are from constant models
#     datasets[[i]] <- dataset[indicator,]
#     PS_scores[[i]] <- PS_scores[[i]][indicator]
#     } else{
#       datasets[[i]] = dataset
#       PS_scores[[i]] = PS_scores[[i]]
#     }
# 
#     IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  datasets[[i]])
#     NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = datasets[[i]])
#   }
# 
#   names(IPW_results) <- paste0("IPW_w_regSetUp", 1:12)
#   names(NIPW_results) <- paste0("NIPW_w_regSetUp", 1:12)
# 
# 
#   estimates <- c(IPW_results, NIPW_results)
# 
#   return(estimates)
# }
# 
# 
# param_list <- list("PSFormula" = c("linear","quadraticNonSymmetric"),
#                    "XImpact" = c("full", "partial"),
#                    "cor" = c("nonCorrelated", "correlated"))
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
# #                                     param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1500,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_59.Rdata")
# 
# 
# 



# sim_VM <- function(n_obs, PS_formula, X_dim, penalty){
#   PS_link = "raw"
#   if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
# 
#   X_impact_share_PS = 0.001
#   X_impact_shift_PS = 0
#   X_impact_share_Y = 0.001
#   X_impact_shift_Y = 0
#   dataset <- gen_DS_modular(n_obs = n_obs,
#                             PS_formula = PS_formula,
#                             PS_link = PS_link,
#                             X_dim = X_dim,
#                             X_impact_share_PS = X_impact_share_PS,
#                             X_impact_shift_PS = X_impact_shift_PS,
#                             X_impact_share_Y = X_impact_share_Y,
#                             X_impact_shift_Y = X_impact_shift_Y)
# 
#   polynomials <- c(0,1,2,3,4,5,6,7,8,9)
#   polynomials_vec <- c()
#   PS_scores <- list()
#   PS_models <- list()
#   for (i in polynomials){
#     polynomials_vec <- c(polynomials_vec, i)
#     output <- Lasso_PS_pred(data = dataset,
#                             penalty = penalty,
#                             polynomials_vector = polynomials_vec,
#                             return_modelAndPS = TRUE,
#                             include_interactions = FALSE)
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
#     #quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
#     quantiles_list <- lapply(PS_scores, quantile, c(0.02,0.98))
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
#     # MISEs <- lapply(PS_models, est_MISE, PS_formula = PS_formula, PS_link = PS_link, cols =1)
#     # SUPs <- lapply(PS_models, est_SUP, PS_formula = PS_formula, PS_link = PS_link, cols =1)
# 
#   names(IPW_results) <- paste0("IPW_", names(IPW_results))
#   names(NIPW_results) <- paste0("NIPW_", names(NIPW_results))
#   # names(IPW_VAR_results) <- paste0("IPW_VAR_", names(IPW_VAR_results))
#   # names(NIPW_coverage) <- paste0("NIPW_coverage_", names(NIPW_coverage))
#   # names(IPW_coverage) <- paste0("IPW_coverage_", names(IPW_coverage))
#   # names(NIPW_VAR_results) <- paste0("NIPW_VAR_", names(NIPW_VAR_results))
#   # names(MISEs) <- paste0("MISE_", names(MISEs))
#   # names(SUPs) <- paste0("SUP_", names(SUPs))
#   names(PS_constant) <- paste0("ConstantPS_", names(PS_constant))
#   estimates <- c(IPW_results, NIPW_results)#, MISEs, SUPs, PS_constant)#, NIPW_VAR_results,
#   #IPW_VAR_results, IPW_coverage, NIPW_coverage)
#   return(estimates)
# }
# 
# #lambdas <- 10^seq(0, -4, by = -.25)
# lambdas <- c(1, 0.5, 0.1, 0.01, 0.005, 0.001, 0.0005, 0.0001)
# 
# param_list <- list("n_obs" = c(200, 700, 2000),#5000),
#                    "X_dim" = c(1000),
#                    "PS_formula"=c("linear"),
#                    "penalty" = c(lambdas))
#                    # "PS_impact" = c("flat", "linear", "quadraticSymmetric",
#                    #                 "quadraticNonSymmetric", "fourthDegree",
#                    #                 "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
#                    #                 "stepNonMonotonic"))
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
#                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 50,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_62.Rdata")
# 
sim_VM <- function(n_obs, PS_formula, X_dim){
  PS_link = "raw"
  if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}

  X_impact_share_PS = 0.002
  X_impact_shift_PS = 0
  X_impact_share_Y = 0.002
  X_impact_shift_Y = 0
  dataset <- gen_DS_modular(n_obs = n_obs,
                            PS_formula = PS_formula,
                            PS_link = PS_link,
                            X_dim = X_dim,
                            X_impact_share_PS = X_impact_share_PS,
                            X_impact_shift_PS = X_impact_shift_PS,
                            X_impact_share_Y = X_impact_share_Y,
                            X_impact_shift_Y = X_impact_shift_Y)

  #Lasso with fixed penalties
  lambdas <- c(1, 0.5, 0.25, 0.1, 0.075, 0.05, 0.04, 0.03, 0.02, 0.01, 0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005, 0.00025)
  fixed_penalty_PS_poly_1 <- list()
  fixed_penalty_PS_poly_2 <- list()
  for (penalty in lambdas){
    PS_poly_1 <- Lasso_PS_pred(data = dataset,
                        penalty = penalty,
                        include_interactions = FALSE)
    PS_poly_2 <- Lasso_PS_pred(data = dataset,
                               penalty = penalty, 
                              polynomials_vector = c(1,2),
                               include_interactions = FALSE)
    fixed_penalty_PS_poly_1 <- append(fixed_penalty_PS_poly_1, list(PS_poly_1))
    fixed_penalty_PS_poly_2 <- append(fixed_penalty_PS_poly_2, list(PS_poly_2))
  }

  fixed_penalty_quantiles_poly_1 <- lapply(fixed_penalty_PS_poly_1, stats::quantile, c(0.02,0.98))
  fixed_penalty_quantiles_poly_2 <- lapply(fixed_penalty_PS_poly_2, stats::quantile, c(0.02,0.98))

  fixed_penalty_IPW_poly_1 <- list()
  fixed_penalty_NIPW_poly_1 <- list()
  fixed_penalty_IPW_poly_2 <- list()
  fixed_penalty_NIPW_poly_2 <- list()
  for (i in 1:length(lambdas)){
    indicator_poly_1 <- (fixed_penalty_PS_poly_1[[i]] > fixed_penalty_quantiles_poly_1[[i]][[1]] & fixed_penalty_PS_poly_1[[i]] < fixed_penalty_quantiles_poly_1[[i]][[2]])
    indicator_poly_2 <- (fixed_penalty_PS_poly_2[[i]] > fixed_penalty_quantiles_poly_2[[i]][[1]] & fixed_penalty_PS_poly_2[[i]] < fixed_penalty_quantiles_poly_2[[i]][[2]])
    #Poly 1
    if (mean(indicator_poly_1) > 0.95) {
      current_dataset <- dataset[indicator_poly_1,]
      fixed_penalty_PS_poly_1[[i]] <- fixed_penalty_PS_poly_1[[i]][indicator_poly_1]
    } else {current_dataset <- dataset}
    fixed_penalty_IPW_poly_1[[i]] <- IPW_fun(PS_est = fixed_penalty_PS_poly_1[[i]], dataset = current_dataset)
    fixed_penalty_NIPW_poly_1[[i]] <- NIPW_fun(PS_est = fixed_penalty_PS_poly_1[[i]], dataset = current_dataset)
    #Poly 2
    if (mean(indicator_poly_2) > 0.95) {
      current_dataset <- dataset[indicator_poly_2,]
      fixed_penalty_PS_poly_2[[i]] <- fixed_penalty_PS_poly_2[[i]][indicator_poly_2]
    } else {current_dataset <- dataset}
    fixed_penalty_IPW_poly_2[[i]] <- IPW_fun(PS_est = fixed_penalty_PS_poly_2[[i]], dataset = current_dataset)
    fixed_penalty_NIPW_poly_2[[i]] <- NIPW_fun(PS_est = fixed_penalty_PS_poly_2[[i]], dataset = current_dataset)
  }
  fixed_penalty_IPW_names_poly_1 <- paste0("IPW_penalty_poly_1_", sub("\\.", "", lambdas))
  fixed_penalty_NIPW_names_poly_1 <- paste0("NIPW_penalty_poly_1_", sub("\\.", "", lambdas))
  fixed_penalty_IPW_names_poly_2 <- paste0("IPW_penalty_poly_2_", sub("\\.", "", lambdas))
  fixed_penalty_NIPW_names_poly_2 <- paste0("NIPW_penalty_poly_2_", sub("\\.", "", lambdas))

  #Cross-validated penalty for lasso, w poly 1
  result_poly_1 <- Lasso_PS_pred(data = dataset,
                          include_interactions = FALSE,
                          return_penalty = TRUE)
  cv_penalty_PS_poly_1 <- result_poly_1[[1]]
  cv_selected_penalty_poly_1 <- result_poly_1[[2]]

  quantile_2_poly_1 <- stats::quantile(cv_penalty_PS_poly_1, c(0.02,0.98))
  indicator_2_poly_1 <- (cv_penalty_PS_poly_1 > quantile_2_poly_1[[1]] & cv_penalty_PS_poly_1 < quantile_2_poly_1[[2]])
  if(mean(indicator_2_poly_1) > 0.95){
    dataset_2_poly_1 <- dataset[indicator_2_poly_1,]
    cv_penalty_PS_poly_1 <- cv_penalty_PS_poly_1[indicator_2_poly_1]
  } else {dataset_2_poly_1 = dataset}

  IPW_result_cv_poly_1 <- IPW_fun(PS_est = cv_penalty_PS_poly_1,
                           dataset = dataset_2_poly_1)
  NIPW_result_cv_poly_1 <- NIPW_fun(PS_est = cv_penalty_PS_poly_1,
                             dataset = dataset_2_poly_1)
  
  ###
  #Cross-validated penalty for lasso, w poly 2
  result_poly_2 <- Lasso_PS_pred(data = dataset,
                                 include_interactions = FALSE,
                                 return_penalty = TRUE,
                                 polynomials_vector = c(1,2))
  cv_penalty_PS_poly_2 <- result_poly_2[[1]]
  cv_selected_penalty_poly_2 <- result_poly_2[[2]]
  
  quantile_2_poly_2 <- stats::quantile(cv_penalty_PS_poly_2, c(0.02,0.98))
  indicator_2_poly_2 <- (cv_penalty_PS_poly_2 > quantile_2_poly_2[[1]] & cv_penalty_PS_poly_2 < quantile_2_poly_2[[2]])
  if(mean(indicator_2_poly_2) > 0.95){
    dataset_2_poly_2 <- dataset[indicator_2_poly_2,]
    cv_penalty_PS_poly_2 <- cv_penalty_PS_poly_2[indicator_2_poly_2]
  } else {dataset_2_poly_2 = dataset}
  
  IPW_result_cv_poly_2 <- IPW_fun(PS_est = cv_penalty_PS_poly_2,
                                  dataset = dataset_2_poly_2)
  NIPW_result_cv_poly_2 <- NIPW_fun(PS_est = cv_penalty_PS_poly_2,
                                    dataset = dataset_2_poly_2)
  
  

  estimates <- c(fixed_penalty_IPW_poly_1, fixed_penalty_NIPW_poly_1, 
                 fixed_penalty_IPW_poly_2, fixed_penalty_NIPW_poly_2, 
                 IPW_result_cv_poly_1, NIPW_result_cv_poly_1,
                 IPW_result_cv_poly_2, NIPW_result_cv_poly_2,
                 cv_selected_penalty_poly_1, cv_selected_penalty_poly_2)
  names(estimates) <- c(fixed_penalty_IPW_names_poly_1, fixed_penalty_NIPW_names_poly_1,
                        fixed_penalty_IPW_names_poly_2, fixed_penalty_NIPW_names_poly_2,
                        "IPW_result_cv_poly_1", "NIPW_result_cv_poly_1", 
                        "IPW_result_cv_poly_2", "NIPW_result_cv_poly_2", 
                        "Penalty_selected_by_CV_poly_1", "Penalty_selected_by_CV_poly_2")
  return(estimates)
}


param_list <- list("n_obs" = c(1000),
                   "X_dim" = c(1000),
                   "PS_formula" = c(#"flat", "linear", "quadraticSymmetric",
                                    "quadraticNonSymmetric",# "fourthDegree",
                                    #"peakSymmetric",
                                    "peakNonSymmetric"))#, "stepMonotonic",
                                    #"stepNonMonotonic"))

# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
#                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)

sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1000,
                                   param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
save(sim_VM_result, file = "sim_final_1_penalty.Rdata")


###
# Testing cross-fitting
###
# 
# sim_VM <- function(n_obs, PS_formula, X_dim){
#   PS_link = "raw"
#   if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
#                         "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
# 
#   X_impact_share_PS = 0.001
#   X_impact_shift_PS = 0
#   X_impact_share_Y = 0.001
#   X_impact_shift_Y = 0
#   dataset <- gen_DS_modular(n_obs = n_obs,
#                             PS_formula = PS_formula,
#                             PS_link = PS_link,
#                             X_dim = X_dim,
#                             X_impact_share_PS = X_impact_share_PS,
#                             X_impact_shift_PS = X_impact_shift_PS,
#                             X_impact_share_Y = X_impact_share_Y,
#                             X_impact_shift_Y = X_impact_shift_Y)
# 
#   split_data <- split_sample(dataset)
#   cross_fitted_estimates <- list()
#   for (i in 1:length(split_data)){
#     vals = cross_fitter(split_data[[i]])
#     cross_fitted_estimates[[i]] = vals
#   }
#   #cross_fitted_estimates <- lapply(split_data, cross_fitter)
#   estimates <- estimate_aggregator(cross_fitted_estimates)
# 
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(400,700,1000,2000),#5000),
#                    "X_dim" = c(1000),
#                    "PS_formula"=c("linear"))
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 10,
# #                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 200,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_61.Rdata")
# 


###
# Testing cross-fitting
###
# 


# sim_VM <- function(n_obs, PS_formula, X_dim, impact_share, issue){
#   PS_link = "raw"
#   if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
#                         "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   
#   #Creating dataset
#   X_impact_shift_PS = 0
#   X_impact_shift_Y = 0
#   X_impact_share_Y = 1/X_dim
#   X_impact_share_PS = 1/X_dim
#   # X_impact_share_Y = impact_share
#   # X_impact_share_PS = impact_share
#   mediator = FALSE
#   unmeasured_PS = FALSE
#   unmeasured_Y = FALSE
#   if (issue == "mediator") {mediator = TRUE}
#   if (issue == "unmeasured_both") {
#     unmeasured_PS = TRUE
#     unmeasured_Y = TRUE}
#   if (issue == "unmeasured_PS") {unmeasured_PS = TRUE}
#   if (issue == "unmeasured_Y") {unmeasured_Y = TRUE}
# 
#   dataset <- gen_DS_modular(n_obs = n_obs,
#                             PS_formula = PS_formula,
#                             PS_link = PS_link,
#                             X_dim = X_dim,
#                             X_impact_share_PS = X_impact_share_PS,
#                             X_impact_shift_PS = X_impact_shift_PS,
#                             X_impact_share_Y = X_impact_share_Y,
#                             X_impact_shift_Y = X_impact_shift_Y,
#                             mediator = mediator,
#                             unmeasured_PS = unmeasured_PS,
#                             unmeasured_Y = unmeasured_Y)
#   ###
#   #Naive regression and regression with relevant covariates
#   ###
#   
#   formula_1 <- as.formula(paste0("Y~T+",paste0(names(dataset)[1:round(X_impact_share_PS*X_dim)], collapse = "+")))
#   reg_est <- coef(lm(Y~T, data = dataset))[2]
#   reg_est_correct_covariates <- coef(lm(formula_1, data = dataset))[2]
#   
#   ###
#   #Correctly specified parametric model, including up to ninth degree of polynomial
#   ###
#   
#   polynomials <- c(0,1,2,3,4,5,6,7,8,9)
#   polynomials_vec <- c()
#   PS_scores_log <- list()
#   subset <- c("T", "OneVector", names(dataset)[round(X_dim*X_impact_share_PS)])
#   dataset_log <- dataset[,subset]
#   
#   for (i in polynomials) {
#     polynomials_vec <- c(polynomials_vec, i)
#     PS_scores_log[[i+1]] <- Log_PS_pred(data = dataset_log, polynomials_vector = polynomials_vec, return_Model = FALSE)
#   }
#   names(PS_scores_log) <- polynomials
#   
#   #Trimming datasets with PS scores within [0.02,0.98] percentiles and estimating TEs
#   datasets_log <- list()
#   IPW_results_log <- list()
#   NIPW_results_log <- list()
#   quantiles_list <- lapply(PS_scores_log, stats::quantile, c(0.02,0.98))
#   #quantiles_list <- lapply(PS_scores_lasso, quantile, c(0.02,0.98))
#   
#   for (i in polynomials+1) {
#     indicator <- (PS_scores_log[[i]] > quantiles_list[[i]][[1]] & PS_scores_log[[i]] < quantiles_list[[i]][[2]])
#     if(mean(indicator > 0.95)){
#       datasets_log[[i]] <- dataset[indicator,]
#       PS_scores_log[[i]] <- PS_scores_log[[i]][indicator]
#     } else {
#       datasets_log[[i]] <- dataset
#       PS_scores_log[[i]] <- PS_scores_log[[i]]
#     }
#     IPW_results_log[[i]] <- IPW_fun(PS_est = PS_scores_log[[i]],dataset =  datasets_log[[i]])
#     NIPW_results_log[[i]] <- NIPW_fun(PS_est = PS_scores_log[[i]], dataset = datasets_log[[i]])
#   }
#   names(IPW_results_log) = polynomials
#   names(NIPW_results_log) = polynomials
#   
#   ###
#   # Lasso as PS estimator, including increasing degrees of polynomials
#   ###
#   
#   polynomials_vec <- c()
#   PS_scores_lasso <- list()
#   IPW_results_lasso <- list()
#   NIPW_results_lasso <- list()
#   
#   for (i in polynomials) {
#     polynomials_vec <- c(polynomials_vec, i)
#     PS_scores_lasso[[i+1]] <- Lasso_PS_pred(data = dataset, polynomials_vector = polynomials_vec)
#   }
#   names(PS_scores_lasso) <- polynomials
#   
#   #Trimming datasets with PS scores within [0.02,0.98] percentiles and estimating TEs
#   datasets_lasso <- list()
#   IPW_results_lasso <- list()
#   NIPW_results_lasso <- list()
#   quantiles_list <- lapply(PS_scores_lasso, stats::quantile, c(0.02,0.98))
#   #quantiles_list <- lapply(PS_scores_lasso, quantile, c(0.02,0.98))
#   
#   for (i in polynomials+1) {
#     indicator <- (PS_scores_lasso[[i]] > quantiles_list[[i]][[1]] & PS_scores_lasso[[i]] < quantiles_list[[i]][[2]])
#     if(mean(indicator > 0.95)){
#       datasets_lasso[[i]] <- dataset[indicator,]
#       PS_scores_lasso[[i]] <- PS_scores_lasso[[i]][indicator]
#     } else {
#       datasets_lasso[[i]] <- dataset
#       PS_scores_lasso[[i]] <- PS_scores_lasso[[i]]
#     }
#     IPW_results_lasso[[i]] <- IPW_fun(PS_est = PS_scores_lasso[[i]],dataset =  datasets_lasso[[i]])
#     NIPW_results_lasso[[i]] <- NIPW_fun(PS_est = PS_scores_lasso[[i]], dataset = datasets_lasso[[i]])
#   }
#   names(IPW_results_lasso) = polynomials
#   names(NIPW_results_lasso) = polynomials
#   
#   # #Matching estimator
#   # explanatory <- names(dataset)[!names(dataset) %in% c("T", "Y", "PS")]
#   # nn_match <- nn_one(PS = PS_pred, dataset = dataset_lassoNCF, treatment = "T", explanatory = explanatory)
#   # nn = lm(Y~T, data = nn_match$matched_data, weights = nn_match$weights)$coefficients[["T1"]]
# 
#   #Double/debiased ML
#   DML_interactive <- DoubleML_treatment_est(data = dataset,
#                                                   model = "interactive",
#                                                   mtry = "half_N")
# 
#   # dml_paper <- coef(rlassoEffect(x=as.matrix(dataset[,1:X_dim]), y = as.matrix(dataset[,X_dim+3]),
#   #                                d = as.matrix(as.numeric(dataset[,X_dim+2])-1), method = "double selection"))["d1"]
# 
# 
#   #Preparing results
#   estimates <- list()
#   estimates <- append(estimates, c(reg_est, reg_est_correct_covariates,
#                                    IPW_results_log, NIPW_results_log,
#                                    IPW_results_lasso, NIPW_results_lasso,
#                                    DML_interactive))
#   names(estimates) <- c("NaiveReg", "RegWcorrectCovariates",
#                         paste0("Log_IPW_",polynomials), paste0("Log_NIPW_",polynomials),
#                         paste0("Lasso_IPW_",polynomials), paste0("Lasso_NIPW_",polynomials),
#                         "DML")
#   
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "X_dim" = c(2000),
#                    "impact_share" = c(0.002),#, 0.004, 0.008, 0.016, 0.032),
#                    "PS_formula"=c(#"flat", "linear", "quadraticSymmetric",
#                                   "quadraticNonSymmetric",# "fourthDegree",
#                                   #"peakSymmetric", 
#                                   "peakNonSymmetric"#, "stepMonotonic",
#                                   #"stepNonMonotonic"
#                                   ),
#                    #"issue" = c("simple", "unmeasured", "mediator"))
#                    "issue" = c("simple", "unmeasured_both")
#                    )
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 30,
# #                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1000,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_final_1_highDim.Rdata")


# 
# sim_VM <- function(n_obs, PS_formula, X_dim, impact_share, issue){
#   PS_link = "raw"
#   if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
#                         "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   
#   #Creating dataset
#   X_impact_shift_PS = 0
#   X_impact_shift_Y = 0
#   # X_impact_share_Y = 1/X_dim
#   # X_impact_share_PS = 1/X_dim
#   X_impact_share_Y = impact_share
#   X_impact_share_PS = impact_share
#   mediator = FALSE
#   unmeasured_PS = FALSE
#   unmeasured_Y = FALSE
#   if (issue == "mediator") {mediator = TRUE}
#   if (issue == "unmeasured_both") {
#     unmeasured_PS = TRUE
#     unmeasured_Y = TRUE}
#   if (issue == "unmeasured_PS") {unmeasured_PS = TRUE}
#   if (issue == "unmeasured_Y") {unmeasured_Y = TRUE}
#   
#   dataset <- gen_DS_modular(n_obs = n_obs,
#                             PS_formula = PS_formula,
#                             PS_link = PS_link,
#                             X_dim = X_dim,
#                             X_impact_share_PS = X_impact_share_PS,
#                             X_impact_shift_PS = X_impact_shift_PS,
#                             X_impact_share_Y = X_impact_share_Y,
#                             X_impact_shift_Y = X_impact_shift_Y,
#                             mediator = mediator,
#                             unmeasured_PS = unmeasured_PS,
#                             unmeasured_Y = unmeasured_Y)
#   ###
#   #Naive regression and regression with relevant covariates
#   ###
#   
#   formula_1 <- as.formula(paste0("Y~T+",paste0(names(dataset)[1:round(X_impact_share_PS*X_dim)], collapse = "+")))
#   reg_est <- coef(lm(Y~T, data = dataset))[2]
#   reg_est_correct_covariates <- coef(lm(formula_1, data = dataset))[2]
#   
#   ###
#   #Correctly specified parametric model, including up to ninth degree of polynomial
#   ###
#   
#   polynomials <- c(0,1,2,3,4,5,6,7,8,9)
#   polynomials_vec <- c()
#   PS_scores_log <- list()
#   subset <- c("T", "OneVector", names(dataset)[round(X_dim*X_impact_share_PS)])
#   dataset_log <- dataset[,subset]
#   
#   for (i in polynomials) {
#     polynomials_vec <- c(polynomials_vec, i)
#     PS_scores_log[[i+1]] <- Log_PS_pred(data = dataset_log, polynomials_vector = polynomials_vec, return_Model = FALSE)
#   }
#   names(PS_scores_log) <- polynomials
#   
#   #Trimming datasets with PS scores within [0.02,0.98] percentiles and estimating TEs
#   datasets_log <- list()
#   IPW_results_log <- list()
#   NIPW_results_log <- list()
#   quantiles_list <- lapply(PS_scores_log, stats::quantile, c(0.02,0.98))
#   #quantiles_list <- lapply(PS_scores_lasso, quantile, c(0.02,0.98))
#   
#   for (i in polynomials+1) {
#     indicator <- (PS_scores_log[[i]] > quantiles_list[[i]][[1]] & PS_scores_log[[i]] < quantiles_list[[i]][[2]])
#     if(mean(indicator > 0.95)){
#       datasets_log[[i]] <- dataset[indicator,]
#       PS_scores_log[[i]] <- PS_scores_log[[i]][indicator]
#     } else {
#       datasets_log[[i]] <- dataset
#       PS_scores_log[[i]] <- PS_scores_log[[i]]
#     }
#     IPW_results_log[[i]] <- IPW_fun(PS_est = PS_scores_log[[i]],dataset =  datasets_log[[i]])
#     NIPW_results_log[[i]] <- NIPW_fun(PS_est = PS_scores_log[[i]], dataset = datasets_log[[i]])
#   }
#   names(IPW_results_log) = polynomials
#   names(NIPW_results_log) = polynomials
#   
#   ###
#   # Lasso as PS estimator, including increasing degrees of polynomials
#   ###
#   
#   polynomials_vec <- c()
#   PS_scores_lasso <- list()
#   IPW_results_lasso <- list()
#   NIPW_results_lasso <- list()
#   
#   for (i in polynomials) {
#     polynomials_vec <- c(polynomials_vec, i)
#     PS_scores_lasso[[i+1]] <- Lasso_PS_pred(data = dataset, polynomials_vector = polynomials_vec)
#   }
#   names(PS_scores_lasso) <- polynomials
#   
#   #Trimming datasets with PS scores within [0.02,0.98] percentiles and estimating TEs
#   datasets_lasso <- list()
#   IPW_results_lasso <- list()
#   NIPW_results_lasso <- list()
#   quantiles_list <- lapply(PS_scores_lasso, stats::quantile, c(0.02,0.98))
#   #quantiles_list <- lapply(PS_scores_lasso, quantile, c(0.02,0.98))
#   
#   for (i in polynomials+1) {
#     indicator <- (PS_scores_lasso[[i]] > quantiles_list[[i]][[1]] & PS_scores_lasso[[i]] < quantiles_list[[i]][[2]])
#     if(mean(indicator > 0.95)){
#       datasets_lasso[[i]] <- dataset[indicator,]
#       PS_scores_lasso[[i]] <- PS_scores_lasso[[i]][indicator]
#     } else {
#       datasets_lasso[[i]] <- dataset
#       PS_scores_lasso[[i]] <- PS_scores_lasso[[i]]
#     }
#     IPW_results_lasso[[i]] <- IPW_fun(PS_est = PS_scores_lasso[[i]],dataset =  datasets_lasso[[i]])
#     NIPW_results_lasso[[i]] <- NIPW_fun(PS_est = PS_scores_lasso[[i]], dataset = datasets_lasso[[i]])
#   }
#   names(IPW_results_lasso) = polynomials
#   names(NIPW_results_lasso) = polynomials
#   
#   # #Matching estimator
#   # explanatory <- names(dataset)[!names(dataset) %in% c("T", "Y", "PS")]
#   # nn_match <- nn_one(PS = PS_pred, dataset = dataset_lassoNCF, treatment = "T", explanatory = explanatory)
#   # nn = lm(Y~T, data = nn_match$matched_data, weights = nn_match$weights)$coefficients[["T1"]]
#   
#   #Double/debiased ML
#   DML_interactive <- DoubleML_treatment_est(data = dataset,
#                                             model = "interactive",
#                                             mtry = "half_N")
#   
#   # dml_paper <- coef(rlassoEffect(x=as.matrix(dataset[,1:X_dim]), y = as.matrix(dataset[,X_dim+3]),
#   #                                d = as.matrix(as.numeric(dataset[,X_dim+2])-1), method = "double selection"))["d1"]
#   
#   
#   #Preparing results
#   estimates <- list()
#   estimates <- append(estimates, c(reg_est, reg_est_correct_covariates,
#                                    IPW_results_log, NIPW_results_log,
#                                    IPW_results_lasso, NIPW_results_lasso,
#                                    DML_interactive))
#   names(estimates) <- c("NaiveReg", "RegWcorrectCovariates",
#                         paste0("Log_IPW_",polynomials), paste0("Log_NIPW_",polynomials),
#                         paste0("Lasso_IPW_",polynomials), paste0("Lasso_NIPW_",polynomials),
#                         "DML")
#   
#   return(estimates)
# }
# 
# 
# param_list <- list("n_obs" = c(1000),
#                    "X_dim" = c(1000),
#                    "impact_share" = c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032),
#                    "PS_formula"=c(#"flat", "linear", "quadraticSymmetric",
#                      "quadraticNonSymmetric",# "fourthDegree",
#                      #"peakSymmetric", 
#                      "peakNonSymmetric"#, "stepMonotonic",
#                      #"stepNonMonotonic"
#                    ),
#                    #"issue" = c("simple", "unmeasured", "mediator"))
#                    "issue" = c("simple")
# )
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 30,
# #                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1000,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_final_1_Covariates.Rdata")
# 


# sim_VM <- function(n_obs, DGP){
#   q = 10
#   p = 100
#   
#   if (DGP == "goodControl") {
#     dataset <- gen_DS_goodControl(n = n_obs, p = p, q = q)
#   } else if (DGP == "simple") {
#     dataset <- gen_DS_MGraph(n = n_obs, p = p, q = q)
#   } else if (DGP == "heterogeneous") {
#     dataset <- gen_DS_MGraph(n = n_obs, p = p, q = q, TE = "heterogeneous")
#   } else if (DGP == "heterogeneous_2") {
#     dataset <- gen_DS_MGraph(n = n_obs, p = p, q = q, TE = "heterogeneous_2")
#   } else if (DGP == "heterogeneous_3") {
#     dataset <- gen_DS_MGraph(n = n_obs, p = p, q = q, TE = "heterogeneous_3")
#   } else if (DGP == "mediator") {
#     dataset <- gen_DS_Mediator(n = n_obs, p = p, q = q)
#   } else if (DGP == "confounded_mediator") {
#     dataset <- gen_DS_Mediator(n = n_obs, p = p, q = q, confounded = TRUE)
#   }
#   
#   #Naive regression
#   reg_est <- coef(lm(Y~T, data = dataset))[2]
#   
#   #Correctly specified parametric model
#   regressors <- names(dataset)[1:q]
#   reg_formula <- as.formula(paste("T ~ ", paste(regressors, collapse= "+")))
#   log_reg <- glm(reg_formula, family = binomial(link = "logit"), data = dataset)
#   PS_pred_log <- as.vector(predict.glm(object = log_reg, newdata = dataset, type = "response"))
#   
#   #quantiles <- quantile(PS_pred_log, c(0.02,0.98))
#   quantiles <- stats::quantile(PS_pred_log, c(0.02,0.98))
#   indicator <- (PS_pred_log > quantiles[[1]] & PS_pred_log < quantiles[[2]])
#   if (mean(indicator) > 0.95) {
#     PS_pred_log <- PS_pred_log[indicator]
#     dataset_log <- dataset[indicator,]
#   } else {dataset_log <- dataset}
#   
#   IPW_log <- IPW_fun(PS_est = PS_pred_log, dataset = dataset_log)
#   NIPW_log <- NIPW_fun(PS_est = PS_pred_log, dataset = dataset_log)
#   
#   #Non-cross-fitted procedure
#   PS_pred <- Lasso_PS_pred(data = dataset,
#                            include_interactions = FALSE)
#   #quantiles <- quantile(PS_pred, c(0.02,0.98))
#   quantiles <- stats::quantile(PS_pred, c(0.02,0.98))
#   indicator <- (PS_pred > quantiles[[1]] & PS_pred < quantiles[[2]])
#   
#   if (mean(indicator) > 0.95) {
#     PS_pred <- PS_pred[indicator]
#     dataset_lassoNCF <- dataset[indicator,]
#   } else {dataset_lassoNCF <- dataset}
#   
#   IPW_results <- IPW_fun(PS_est = PS_pred, dataset =  dataset_lassoNCF)
#   NIPW_results <- NIPW_fun(PS_est = PS_pred, dataset =  dataset_lassoNCF)
#   
#   #Double/debiased ML
#   DML_interactive_halfN <- DoubleML_treatment_est(data = dataset,
#                                                   model = "interactive",
#                                                   mtry = "half_N")
#   # DML_pLinear_halfN <- DoubleML_treatment_est(data = dataset,
#   #                                             model = "partiallyLinear",
#   #                                             mtry = "half_N")
#   
#   #Approach from 2022 paper with their dml package and post-lasso
#   dml_paper <- coef(rlassoEffect(x=as.matrix(dataset[,1:p]), y = as.matrix(dataset[,p+3]),
#                                  d = as.matrix(as.numeric(dataset[,p+2])-1), method = "double selection"))["d1"]
#   # form <- as.formula(paste0("Y~", paste0(names(dataset)[1:p], collapse = "+"),"+T"))
#   # post.lasso <- coef(rlasso(form, post = T, intercept = F, data = dataset))["T1"]
#   
#   #Preparing results
#   estimates <- list()
#   estimates <- append(estimates, c(reg_est, 
#                                    IPW_log, NIPW_log, 
#                                    dml_paper, #post.lasso,
#                                    IPW_results, NIPW_results, 
#                                    DML_interactive_halfN))
#   names(estimates) <- c( "NaiveReg", 
#                          "Log_IPW", "Log_NIPW",
#                          "DML_paper", #"Post_Lasso",
#                          "Lasso_IPW", "Lasso_NIPW",  
#                          "DML"
#   )
#   
#   return(estimates)
#   
# }
# 
# 
# param_list <- list("n_obs" = c(100, 300, 500, 1000),
#                    "DGP" = c("goodControl",
#                      "simple", 
#                              "heterogeneous", "heterogeneous_2", "heterogeneous_3", 
#                              "mediator", "confounded_mediator"))
# 
# # sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
# #                                    param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)
# 
# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 300,
#                                    param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
# save(sim_VM_result, file = "sim_VM_78.Rdata")
# 



