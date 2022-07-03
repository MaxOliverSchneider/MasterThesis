#setwd("C:/Users/max/Documents/Master/FS 21/MA/Fr?lich/Code/MA")

#Might imporve error logging on VM
options(keep.source = TRUE)

#Load packages
packages <- c("MonteCarlo", "snowfall", "tryCatchLog")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R", "Metrics.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))



# sim_VM <- function(PS_formula, n_obs){
#   PS_link = "raw"
#   if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
#                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
#   X_dim = 1
#   dataset <- gen_DS_modular(n_obs = n_obs, PS_formula= PS_formula, PS_link = PS_link, X_dim = X_dim, X_impact_share_PS = 1)
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
#     output <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec, return_Model = TRUE)
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
#   Kernel_002 <- list()
#   Kernel_010 <- list()
#   Kernel_025 <- list()
#   Kernel_050 <- list()
# 
# 
#   quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
# 
#   for (i in polynomials+1){
# 
#     #indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
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
#     indicator <- enforce_overlap(dataset, PS_scores[[i]])
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
#     Kernel_002[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#                                   bandwidth = 0.02)
#     Kernel_010[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#                                   bandwidth = 0.10)
#     Kernel_025[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#                                   bandwidth = 0.25)
#     Kernel_050[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
#                                   bandwidth = 0.50)
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
#   names(Kernel_002) <- paste0("Kernel_bw002_w_poly", polynomials)
#   names(Kernel_010) <- paste0("Kernel_bw010_w_poly", polynomials)
#   names(Kernel_025) <- paste0("Kernel_bw025_w_poly", polynomials)
#   names(Kernel_050) <- paste0("Kernel_bw050_w_poly", polynomials)
#   # names(IPW_MSEover1k) <- paste0("IPW_w_poly_exceed", polynomials)
#   # names(NIPW_MSEover1k) <- paste0("NIPW_w_poly_exceed", polynomials)
#   names(MISEs) <- paste0("MISE_w_poly_", polynomials)
#   names(SUPs) <- paste0("SUP_w_poly_", polynomials)
# 
#   estimates <- c(IPW_results, NIPW_results,
#                  Kernel_002, Kernel_010, Kernel_025, Kernel_050,
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
# save(sim_VM_result, file = "sim_VM_55.Rdata")

sim_VM <- function(PS_formula, n_obs, XImpact, cor){
  PS_link = "raw"
  if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
                        "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
  X_dim = 3
  if(cor == "correlated"){X_dim = "correlated"}

  if(XImpact == "full") {
    X_impact_share_PS = 1
    X_impact_shift_PS = 0
    X_impact_share_Y = 1
    X_impact_shift_Y = 0
  } else if(XImpact == "partial") {
    X_impact_share_PS = 2/3
    X_impact_shift_PS = 0
    X_impact_share_Y = 2/3
    X_impact_shift_Y = 1/3
  }
  dataset <- gen_DS_modular(n_obs = n_obs,
                            PS_formula = PS_formula,
                            PS_link = PS_link,
                            X_dim = X_dim,
                            X_impact_share_PS = X_impact_share_PS,
                            X_impact_shift_PS = X_impact_shift_PS,
                            X_impact_share_Y = X_impact_share_Y,
                            X_impact_shift_Y = X_impact_shift_Y,
                            adjust_alpha_Y = 1,
                            adjust_standard_deviation_Y = 0.5)

  PS_scores <- list()
  PS_models <- list()


  for (y in c(0,6)) {
    if (y == 0) {vars_to_exclude = c("Y", "PS")} else {vars_to_exclude = c("Y", "PS", "X3")}
    out <- Log_PS_pred(data = dataset,
                       polynomials_vector = c(0),
                       return_Model = TRUE,
                       vars_to_exclude = vars_to_exclude)
    PS_scores[[1+y]] <- out[[1]]
    PS_models[[1+y]] <- out[[2]]
    out <- Log_PS_pred(data = dataset,
                       return_Model = TRUE,
                       vars_to_exclude = vars_to_exclude)
    PS_scores[[2+y]] <- out[[1]]
    PS_models[[2+y]] <- out[[2]]
    out <- Log_PS_pred(data = dataset,
                       interaction_terms = c(1),
                       return_Model = TRUE,
                       vars_to_exclude = vars_to_exclude)
    PS_scores[[3+y]] <- out[[1]]
    PS_models[[3+y]] <- out[[2]]
    out <- Log_PS_pred(data = dataset,
                       interaction_terms = c(1),
                       polynomials_vector = c(1,2),
                       return_Model = TRUE,
                       vars_to_exclude = vars_to_exclude)
    PS_scores[[4+y]] <- out[[1]]
    PS_models[[4+y]] <- out[[2]]
    out <- Log_PS_pred(data = dataset,
                       interaction_terms = c(1,2),
                       polynomials_vector = c(1,2),
                       return_Model = TRUE,
                       vars_to_exclude = vars_to_exclude)
    PS_scores[[5+y]] <- out[[1]]
    PS_models[[5+y]] <- out[[2]]
    out <- Log_PS_pred(data = dataset,
                       interaction_terms = c(1,2),
                       polynomials_vector = c(1,2,3),
                       return_Model = TRUE,
                       vars_to_exclude = vars_to_exclude)
    PS_scores[[6+y]] <- out[[1]]
    PS_models[[6+y]] <- out[[2]]
  }

  #Trimming and calculating treatment effects in same loop
  datasets <- list()
  IPW_results <- list()
  NIPW_results <- list()


  quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))

  for (i in (1:length(PS_scores))){

    indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])

    ###
    #ATTENTION
    ####

    # #Get indicator from true PS
    # if (PS_formula != "flat"){
    #   indicator <- stats::quantile(dataset$PS, c(0.02,0.98))} else {indicator = rep(TRUE, nrow(dataset))}
    #
    # ####
    # ####
    # ###
    if (!i %in% c(1,7)) { #Dont do it if PS scores are from constant models
    datasets[[i]] <- dataset[indicator,]
    PS_scores[[i]] <- PS_scores[[i]][indicator]
    } else{
      datasets[[i]] = dataset
      PS_scores[[i]] = PS_scores[[i]]
    }

    IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  datasets[[i]])
    NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = datasets[[i]])
  }

  names(IPW_results) <- paste0("IPW_w_regSetUp", 1:12)
  names(NIPW_results) <- paste0("NIPW_w_regSetUp", 1:12)


  estimates <- c(IPW_results, NIPW_results)#,

  return(estimates)
}


param_list <- list("n_obs" = c(1000),
                   "PS_formula" = c("linear","quadraticNonSymmetric"),
                   "XImpact" = c("full", "partial"),
                   "cor" = c("nonCorrelated", "correlated"))

# sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
#                                     param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)

sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1500,
                                   param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
save(sim_VM_result, file = "sim_VM_57.Rdata")



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

