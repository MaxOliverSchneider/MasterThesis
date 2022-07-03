scripts = c("Graphs.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

simNoToLoad <- "56"
load(paste0("~/Master/FS 21/MA/Frölich/Code/MA/results/sim_VM_",simNoToLoad, ".Rdata"))

summary(sim_VM_result)
params = names(sim_VM_result$param_list)

#Create output with means
MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=2, include_meta=FALSE)
MakeTable(output=sim_VM_result, rows=c("list"),
          cols=c("XImpact", "cor", "PS_formula"), digits=2, include_meta=FALSE)

#Create output with standard deviations
#Need to set number of estimators
no_estimators <- length(sim_VM_result[[1]]) 



#Add 0.69 to obtain bias
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x+0.69)
#Subtract 1.27 in three-dimensional case, 0.29 in two-dimensional case
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x-1.27)
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x-0.29)
#Multiply by 1000 to get better magnitude
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x*1000)
#Get absolute value for mean absolute bias
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) abs(x))
#Square to obtain MSE
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x^2)

MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=3, include_meta=FALSE, collapse = as.list(rep("sd", no_estimators)))

#Custom ordering of columns
MakeTable(output=sim_VM_result, rows="list", 
          cols=c("alpha_PS", "beta_PS", "cor_X"), digits=2, include_meta=FALSE)
MakeTable(output=sim_VM_result, rows="list", 
          cols=c("alpha_PS","PS_link", "n_obs", "alpha_outcome" ), digits=1, include_meta=FALSE)


#Probably needs some modification to show distribution of one particular set-up
dist_bias_sim_result(sim_VM_result, params = params)

#Create custom outputs, with different metrics
#In array sim_VM_result[[1]][[16]] contains all results for one estimator
#This is saved in multi-dimensonal array like [j,k,y] where j&k are indicators of the
#parameter set-up and y indicator of a specific replication
#E.g. (sim_VM_result[[1]][[16]][3,1,1:100]) would show all results for one estimator,
#and one parameter set up for replications 1 to 100
