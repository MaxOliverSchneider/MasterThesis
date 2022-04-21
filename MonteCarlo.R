Sys.setenv(LANG = "en")

#Load packages
packages <- c("MonteCarlo", "snowfall")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

sim_x <- function(n_obs, n_A, n_B, n_C) {
  dataset = genDSmultivariate_1(n_obs = n_obs,
                                n_A = n_A,
                                n_B = n_B,
                                n_C = n_C,
                                treatment_effect = 20,
                                v_range = c(-0.3, 0.3), 
                                alpha_PS = 5)
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  return(TE_effects)
}

sim_1 <- function(n_obs) {
  dataset = genDSmultivariate_2(n_obs = n_obs,
                                treatment_effect = 20,
                                v_range = c(-0.3, 0.3), 
                                alpha_PS = 5)
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  return(TE_effects)
}

n_obs = c(500, 1000)
param_list = list("n_obs" = n_obs)
sim_4_result <- MonteCarlo(func = sim_2, nrep = 10, param_list = param_list)
MakeTable(output=sim_4_result, rows="list", cols=c("n_obs"), digits=2, include_meta=FALSE)
save(sim_4_result, file = "scenario1.Rdata")

#Simple set-up
sim_2 <- function(n_obs) {
  dataset = genDS_MV_simple()
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
}

n_obs = c(500, 1000)
param_list = list("n_obs" = n_obs)
sim_2_result <- MonteCarlo(func = sim_2, nrep = 10, param_list = param_list, time_n_test = TRUE)
MakeTable(output=sim_2_result, rows="list", cols=c("n_obs"), digits=2, include_meta=FALSE)
save(sim_2_result, file = "scenario2.Rdata")

#Simple set-up, with noise in PS_score
sim_3 <- function(n_obs) {
  dataset = genDS_MV_simple(n_obs = 500, v_range = c(-1,1), treatment_effect = 10)
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  bias <- lapply(TE_effects, function(x) x - 10)
  return(bias)
}

param_list = list("n_obs" = 500)
sim_3_result <- MonteCarlo(func = sim_3, nrep = 10, param_list = param_list, time_n_test = TRUE)
MakeTable(output=sim_3_result, rows="list", cols=c("n_obs"), digits=2, include_meta=FALSE)
save(sim_3_result, file = "scenario3.Rdata")

#Simple set-up, with noise in treatment impact
sim_4 <- function(n_obs) {
  dataset = genDS_MV_simple(n_obs = 500, e_range = c(-1,1), treatment_effect = 10)
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  bias <- lapply(TE_effects, function(x) x - 10)
  return(bias)
}

param_list = list("n_obs" = 500)
sim_4_result <- MonteCarlo(func = sim_4, nrep = 10, param_list = param_list, time_n_test = TRUE)
MakeTable(output=sim_4_result, rows="list", cols=c("n_obs"), digits=2, include_meta=FALSE)
save(sim_4_result, file = "scenario4.Rdata")

###
# Simulation set-up with different DGPS
###
sim_different_set_ups <- function(n_obs, DGPS) {
  if (DGPS == "simple"){
    dataset = genDS_MV_simple(n_obs = n_obs)}
  else if (DGPS == "PS_error"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              v_range = c(-1,1))}
  else if (DGPS == "Y_error"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              e_range = c(-1,1))}
  else if (DGPS == "high_dim"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              n_X = 150)}
  else if (DGPS == "x_on_y"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              alpha_outcome = 2)}
  else if (DGPS == "multi_col"){
    dataset = genDS_MV_simple(n_obs = n_obs,
                              X_dist = "normal_cor")}
  PS_scores <- PS_estimators(dataset, estimators = c("log", "ridge", "lasso", "rf"), target_var = "T", vars_to_exclude = c("Y", "PS_scaled"))
  matched_data <- matching_function(PS_scores = PS_scores,
                                    dataset = dataset,
                                    matching_estimators = c("nn_one", "nnk")) #issue with caliper matching"stratum",,  "cs"
  TE_effects <- estimate_TE(matched_data)
  bias <- lapply(TE_effects, function(x) x - 10)
  return(bias)
}

param_list = list("n_obs" = c(200,500, 1000), 
                  "DGPS" = c("simple", "PS_error", "Y_error", "high_dim", 
                             "x_on_y", "multi_col"))

sim_result <- MonteCarlo(func = sim_different_set_ups, nrep = 10, param_list = param_list, time_n_test = TRUE)
MakeTable(output=sim_VM_result, rows="list", cols=c("n_obs"), digits=2, include_meta=FALSE)
save(sim_4_result, file = "scenario4.Rdata")

test <- lm(Y~T, data = dataset)
test
###
# Generate table of results
###

#MakeTable(output=sim_1_result, rows="n_obs", cols=c("n_A", "n_B", "n_C"), digits=2, include_meta=FALSE)
MakeTable(output=sim_VM_result, rows="list", cols=c("var", "alpha_PS", "alpha_outcome"), digits=1, include_meta=FALSE)
MakeTable(output=sim_VM_result, rows="list", cols=c("n_obs", "alpha_PS", "Y_error", "DGP"), digits=1, include_meta=FALSE)
#Can output standard deviation (or other metric) of results instead of their mean
MakeTable(output=sim_VM_result, rows="list", cols=c("n_obs"), digits=2, include_meta=FALSE, collapse = list("sd", "sd", "sd", "sd","sd", "sd", "sd", "sd"))
save(sim_4_result, file = "results/scenario1.Rdata")

#Learnings
#Output der Funktion darf nur Liste mit Namen ohne ' sein

#Beim Testen des Fehlers einfachste Form möglich nehmen und dann immer weitere
#Teile hinzufügen

#For parallelization, have all functions available in workspace

#Bei kleiner Beobachtungszahl (und bspw. extremerer Verteilung des PS) 
#kann es passieren dass sich bspw bei treatment nur eine der zwei Ausprägungen im
#Datensatz vorfinden