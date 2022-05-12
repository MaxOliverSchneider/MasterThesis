scripts = c("GenerateData.R", "ModelImplementations.R", "MatchingImplementation.R", "Graphs.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

###
#Testing performance of the PS estimators
###
results = c()
for (i in c(1,5,30)) {
  PS_impact = "interaction"
  dataset <- gen_DS_modular(PS_impact = PS_impact, X_dim = i)
  
  #Determine which estimators to use
  PS_est <- c("log","ridgeNaive", "ridgeInterPoly", 
              "lassoNaive", "lassoInterPoly", "bayes_ridgeInterPoly", "bayes_lassoInterPoly", "rf", "nn")
  
  PS_scores <- PS_estimators(dataset,
                             estimators = PS_est,
                             target_var = "T",
                             vars_to_exclude = c("Y", "PS"))
  
  #Give share of correct classifications if PS>0.5 --> T = 1
  results = rbind(results,PS_predictive_performance(dataset, PS_scores))
  #Plot predicted vs. true PS scores
  output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
  do.call("grid.arrange", c(output, top = paste0("X_dim = ", i," PS_impact = ", PS_impact)))
}
#do.call("grid.arrange", output)

dataset <- gen_DS_modular(
  PS_impact = "kink", to_interact_PS = list(c(1,2),c(3,4),c(5,6)), Y_impact = "linear", n_obs = 1000,
  X_dim = 10, X_impact_share_PS = 0.7, X_covar = 0.5, X_impact_share_Y = 0.7, 
  X_impact_shift_percent = 0.3, adjust_beta_PS = TRUE, beta_adjust_power_PS = 0.1, adjust_alpha_Y = TRUE,
  alpha_PS = 0.65)
