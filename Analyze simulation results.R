scripts = c("Graphs.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

# Load required packages
packages <- c("gdata")
lapply(packages, require, character.only = TRUE)

simNoToLoad <- "73"
load(paste0("~/Master/FS 21/MA/Frölich/Code/MA/results/sim_VM_",simNoToLoad, ".Rdata"))
load(paste0("~/Master/FS 21/MA/Frölich/Code/MA/results/sim_final_",simNoToLoad, ".Rdata"))
MergeResults(path = "~/Master/FS 21/MA/Frölich/Code/MA/results/", identifier = "sim_final_1")

test <- sim_VM_result
test2 <- sim_VM_result

summary(sim_VM_result)
params = names(sim_VM_result$param_list)


#Add 0.69 to obtain bias
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x+0.69)
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x-0.29)
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x-1)
#Subtract 1.27 in three-dimensional case, 0.29 in two-dimensional case
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x-1.27)
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x-0.29)
#Multiply by 1000 to get better magnitude
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x*1000)
#Get absolute value for mean absolute bias
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) abs(x))
#Square to obtain MSE
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x^2)
#Multiply by root n to get convergence rate
sim_VM_result[[1]] <- lapply(sim_VM_result[[1]], function(x) x*(1000^0.5))

###
# Making the scaling by root n flexible
###

# First element in sim_VM_result are results (indexed by double brackets)
# Second element is each return element from simulation (normally the different estimators) (indexed by double brackets)
# Third element is multi-dimensional touble, each element corresponds to range of parameter in simulation (e.g. n_obs, X_dim, etc.)
# Here, the element determining sample size needs to be found. This is then indexed by y. The appropriate sample sizes need
# to be set in n_obs_sim. The last element of the touple indexes the repetitions and should be adapted to include all repetitions.
n_obs_sim <- c(250,500,1000)
n_rep <- 500
for (i in 1:length(sim_VM_result[[1]])){
  for (y in 1:length(n_obs_sim)){
    sim_VM_result[[1]][[i]][y,1:2,1:3,1,1:2,1:n_rep] <- sim_VM_result[[1]][[i]][y,1:2,1:3,1,1:2,1:n_rep]*(n_obs_sim[y]^0.5)
  }
}

###
# Creating tables
###

#Create output with means
MakeTable(output=sim_VM_result, rows=c(params),
          cols="list", digits=2, include_meta=FALSE)
MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=2, include_meta=FALSE,
          partial_grid = list("PSFormula" = c(1,2), "XImpact" = c(1,2), "cor"=c(1,2)))
MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=2, include_meta=FALSE,
          partial_grid = list("penalty" = c(1), "X_dim" = c(1), "PS_formula"=c(1), "n_obs"=c(1,3)))

partial_grid=list("n"=c(1,3), "loc"=c(1,3,5))

MakeTable(output=sim_VM_result, rows="list",
          cols=c(params), digits=2, include_meta=FALSE,
          partial_grid = list("n_obs" = c(3)))


MakeTable(output=sim_VM_result, rows=c("list"),
          cols=c("XImpact", "cor", "PS_formula"), digits=2, include_meta=FALSE)



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

#Subsetting results to only look at a certain subset, since partial_grid is not working
#In subset vector, set ranges of variables you want to look at, last value describes
#numbers of repetitions that should be included
sim_VM_result[[1]][[1]][1,1,1,1, 1, 1:10]


###
# Creating outputs from custom subset of data 
###
data <- MakeFrame(sim_VM_result)
dt <- data %>% filter(X_dim == 1000) %>%
  filter(n_obs == 1000) %>%
  filter(issue == "issue=unmeasured_both") %>%
  select(starts_with("Lasso") | c("PS_formula", "DML")) %>%
  #select(starts_with("Log") | c("PS_formula")) %>%
  group_by(PS_formula) %>%
  summarise(across(everything(), mean))

# This command is not working
# dt$PS_formula  <- reorder.factor(dt$PS_formula , 
#                                  levels = c("PS_formula=quadraticNonSymmetric","peakNonSymmetric"))
dt <- dt[order(dt$PS_formula, decreasing = TRUE),]  
dt[,2:ncol(dt)] <- round(dt[,2:ncol(dt)], digits = 2)
stargazer(dt, summary = FALSE)


