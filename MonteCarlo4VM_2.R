#Load packages
packages <- c("MonteCarlo", "snowfall")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

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
  if (DGP == "kink_linear") {PS_impact = "kink"}
  if (DGP == "kink_2_linear") {PS_impact = "kink_2"}
  
  dataset <- gen_DS_modular(
    PS_impact = PS_impact, Y_impact = Y_impact, n_obs = n_obs,
    X_dim = 10, X_impact_share_PS = 0.7, X_covar = 0.5, X_impact_share_Y = 0.7, 
    X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, adjust_alpha_Y = TRUE)
  #Check for excessive mean diff ins PS scores
  mean_diff <- mean(dataset$PS[dataset$T == 1]) - mean(dataset$PS[dataset$T == 0])
  PS_est <- c("true", "log", "probit", "ridgeNaive", "ridgeInterPoly", 
              "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
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
    
  }
  return(bias)
}

param_list <- list("n_obs" = c(1000,3000),
                   "DGP" = c( "quadratic_linear", "quadratic2_linear", "quadratic3_linear",
                              "linear_quadratic", "quadratic_quadratic", "kink", "kink_2"))

sim_VM_result <- tryLog(MonteCarlo(func = sim_VM, nrep = 25,
                                   param_list = param_list, time_n_test = FALSE, ncpus = 16), write.error.dump.file = TRUE)
save(sim_VM_result, file = "sim_VM_21.Rdata")