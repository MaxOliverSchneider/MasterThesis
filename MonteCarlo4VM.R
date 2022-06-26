#setwd("C:/Users/max/Documents/Master/FS 21/MA/Fr?lich/Code/MA")

#Might imporve error logging on VM
options(keep.source = TRUE)

#Load packages
packages <- c("MonteCarlo", "snowfall", "tryCatchLog")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R", "Metrics.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))



sim_VM <- function(PS_formula, n_obs){
  PS_link = "raw"
  if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
                       "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
  X_dim = 1
  X_impact_share_PS = 1
  dataset <- gen_DS_modular(n_obs = n_obs, PS_formula= PS_formula, PS_link = PS_link, X_dim = X_dim, X_impact_share_PS = 1)
  polynomials <- c(0,1,2,3,4,5,6,7,8,9)
  polynomials_vec <- c()
  PS_scores <- list()
  PS_models <- list()
  #Now checking how it behaves when trimming on true PS
  #quantiles <- quantile(dataset$PS, c(0.02,0.98))
  
  for (i in polynomials){
    polynomials_vec <- c(polynomials_vec, i)
    
    output <- Log_PS_pred(data = dataset, polynomials_vector = polynomials_vec, return_Model = TRUE)
    
    PS_scores[[i+1]] <- output[[1]]
    PS_models[[i+1]] <- output[[2]]
  }
  names(PS_scores) <- polynomials
  names(PS_models) <- polynomials

  #Trimming and calculating treatment effects in same loop
  datasets <- list()
  IPW_results <- list()
  NIPW_results <- list()
  IPW_VAR_results <-list()
  NIPW_VAR_results <-list()
  IPW_coverage <- list()
  NIPW_coverage <- list()
  Kernel_002 <- list()
  Kernel_010 <- list()
  Kernel_025 <- list()
  Kernel_050 <- list()


  quantiles_list <- lapply(PS_scores, stats::quantile, c(0.02,0.98))
  
  for (i in polynomials+1){
    
    #indicator <- (PS_scores[[i]] > quantiles_list[[i]][[1]] & PS_scores[[i]] < quantiles_list[[i]][[2]])
    
    ###
    #ATTENTION
    ####
    
    #Get indicator from true PS
    if (PS_formula != "flat"){
    indicator <- stats::quantile(dataset$PS, c(0.02,0.98))} else {indicator = rep(TRUE, nrow(dataset))}
    
    ####
    ####
    ###
    
    if(i>1){ #Because trimming is not working with a constant PS score
      datasets[[i]] <- dataset[indicator,]
      PS_scores[[i]] <- PS_scores[[i]][indicator]
    } else {
      datasets[[i]] <- dataset
      PS_scores[[i]] <- PS_scores[[i]]
    }  
    IPW_results[[i]] <- IPW_fun(PS_est = PS_scores[[i]],dataset =  datasets[[i]])
    NIPW_results[[i]] <- NIPW_fun(PS_est = PS_scores[[i]], dataset = datasets[[i]])
    Kernel_002[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
                                  bandwidth = 0.02)
    Kernel_010[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
                                  bandwidth = 0.10)
    Kernel_025[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
                                  bandwidth = 0.25)
    Kernel_050[[i]] <- Kernel_fun(dataset = datasets[[i]], PS_est = PS_scores[[i]],
                                  bandwidth = 0.50)
    
  }

  #MISE & SUP
  MISEs <- lapply(PS_models, est_MISE, PS_formula = PS_formula, PS_link = PS_link, cols =1)
  SUPs <- lapply(PS_models, est_SUP, PS_formula = PS_formula, PS_link = PS_link, cols =1)
  
  
  names(IPW_results) <- paste0("IPW_w_poly", polynomials)
  names(NIPW_results) <- paste0("NIPW_w_poly", polynomials)
  names(Kernel_002) <- paste0("Kernel_bw002_w_poly", polynomials)
  names(Kernel_010) <- paste0("Kernel_bw010_w_poly", polynomials)
  names(Kernel_025) <- paste0("Kernel_bw025_w_poly", polynomials)
  names(Kernel_050) <- paste0("Kernel_bw050_w_poly", polynomials)
  # names(IPW_MSEover1k) <- paste0("IPW_w_poly_exceed", polynomials)
  # names(NIPW_MSEover1k) <- paste0("NIPW_w_poly_exceed", polynomials)
  names(MISEs) <- paste0("MISE_w_poly_", polynomials)
  names(SUPs) <- paste0("SUP_w_poly_", polynomials)
  
  estimates <- c(IPW_results, NIPW_results, 
                 Kernel_002, Kernel_010, Kernel_025, Kernel_050,
                 MISEs, SUPs)
  
  return(estimates)
}


param_list <- list("n_obs" = c(1000),
                   "PS_formula" = c("flat", "linear", "quadraticSymmetric",
                                   "quadraticNonSymmetric", "fourthDegree",
                                   "peakSymmetric", "peakNonSymmetric", "stepMonotonic",
                                   "stepNonMonotonic"))

sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 1,
                                   param_list = param_list, time_n_test = FALSE), write.error.dump.file = TRUE)

sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 500,
                                   param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
save(sim_VM_result, file = "sim_VM_51.Rdata")


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

