scripts = c("Graphs.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

simNoToLoad <- 20
load(paste0("~/Master/FS 21/MA/Frölich/Code/MA/results/sim_VM_",simNoToLoad, ".Rdata"))

summary(sim_VM_result)
params = names(sim_VM_result$param_list)

#Create output with means
MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=1, include_meta=FALSE)

#Create output with standard deviations
#Need to set number of estimators
no_estimators <- 53 #Taken from summary under parameter grid X ouput arrays of dimensions: ...
MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=1, include_meta=FALSE, collapse = as.list(rep("sd", no_estimators)))

#Custom ordering of columns
MakeTable(output=sim_VM_result, rows="list", 
          cols=c("alpha_PS", "beta_PS", "cor_X"), digits=2, include_meta=FALSE)
MakeTable(output=sim_VM_result, rows="list", 
          cols=c("alpha_PS","PS_link", "n_obs", "alpha_outcome" ), digits=1, include_meta=FALSE)


#Probably needs some modification to show distribution of one particular set-up
dist_bias_sim_result(sim_VM_result, params = params)
