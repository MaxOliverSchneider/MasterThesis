#Might imporve error logging on VM
options(keep.source = TRUE)

#Load packages
packages <- c("MonteCarlo", "snowfall", "tryCatchLog", "foreach", "doParallel")
lapply(packages, require, character.only = TRUE)

scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

#Name packages required
required_packages <- c("MASS", "scales", "clusterGeneration", "tidyverse","tidymodels", "glmnet", "workflows", "tune", "magrittr", 
                       "mlr3", "mlr3learners", "data.table", "bayesreg", "keras", "DoubleML", "MatchIt")

cl <- makeCluster(16)
registerDoParallel(cl)

results <- vector("list", length = 2)

nvars <- c(10,100)
#Testing with share of X_vars without impact
results <- try(foreach(i=1:2,
                   .packages = required_packages) %dopar% {
  dataset <- gen_DS_modular(X_dim = nvars[i],
                            X_impact_share_PS = 0.5,
                            X_impact_share_Y = 0.6,
                            X_impact_shift_percent_Y = 0.1,
                            adjust_beta_PS = TRUE,
                            beta_adjust_power_PS = 0.5, 
                            n_obs = 500)
  # Determine which estimators to use
  PS_est <- c("log","ridgeNaive", "ridgeInterPoly",
              "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
  
  PS_scores <- PS_estimators(dataset,
                             estimators = PS_est,
                             target_var = "T",
                             vars_to_exclude = c("Y", "PS"))
  result <- list()
  result[[1]] <- dataset
  result[[2]] <- PS_scores
  result
}
save(results, file = "PS_estimator_results/03NoImpact.Rdata")
)



