#Load packages
packages <- c("MonteCarlo", "snowfall")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

sim_VM <- function(n_obs, DGPS) {
  if (DGPS == "simple"){
    dataset = genDS_MV_simple(n_obs = n_obs)}
  else if (DGPS == "PS_error"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              v_range = c(-1,1))}
  else if (DGPS == "Y_error"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              e_range = c(-1,1))}
  else if (DGPS == "400var"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              n_X = 400)}
  else if (DGPS == "250var"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              n_X = 250)}
  else if (DGPS == "x_on_y"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              alpha_outcome = 2)}
  else if (DGPS == "multi_col"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              X_dist = "normal_cor")}
  else if (DGPS == "non_linear"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              PS_ling = "non_linear")}
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  bias <- lapply(TE_effects, function(x) x - 10)
  return(bias)
}

param_list = list("n_obs" = c(200,500, 1000), 
                  "DGPS" = c("250var", "400var", "x_on_y", "multi_col", "non_linear"))
sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 15, param_list = param_list, time_n_test = TRUE, ncpus = 16)
save(sim_VM_result, file = "sim_VM.Rdata")

