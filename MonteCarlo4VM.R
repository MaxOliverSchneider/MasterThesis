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
  else if (DGPS == "nonLinear_1"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              PS_link = "nonLinear_1")}
  else if (DGPS == "het_TE"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              Y_link = "het_TE")}
  else if (DGPS == "many_irrelevant_X"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              PS_estimation = "many_irrelevant_X",
                              n_X = 200)}
  else if (DGPS == "lechner"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              treatment_assignment = "lechner")}
  else if (DGPS == "lechner"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              X_dist = "two_norm_dist")}
  
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  bias <- lapply(TE_effects, function(x) x - 10)
  return(bias)
}

param_list = list("n_obs" = c(200, 1000), 
                  "DGPS" = c("lechner", "two_norm_dist"))
sim_VM_result <- MonteCarlo(func = sim_VM, nrep = 15, param_list = param_list, time_n_test = TRUE, ncpus = 16)
save(sim_VM_result, file = "sim_VM_2.Rdata")

