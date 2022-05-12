# Load required packages
packages <- c("MatchIt")
lapply(packages, require, character.only = TRUE)


###
# Matching function
###

matching_function <- function(PS_scores,
                              dataset,
                              treatment = "T",
                              vars_to_exclude = c("Y", "PS"),
                              matching_estimators = c("nn_one", "nnk", "stratum", "caliper_est", "cs"),
                              ratio = 2, #number of neighbors for knn
                              subclass = 6, #determines number of subclasses used for stratum matching
                              min.n = 1, #determines minimum of observation in subclass in stratum matching
                              n_caliper = 0.2) {
  #Function includes all possible matching estimators and uses list of estimated PS scores
  matching_results = list()
  explanatory <- names(dataset)[!names(dataset) %in% c(treatment, vars_to_exclude)]
  
  if ("nn_one" %in% matching_estimators) {
    matching_results[["nn_one"]] <- lapply(PS_scores, nn_one, dataset = dataset, treatment = treatment, explanatory = explanatory) 
  }
  if ("nnk" %in% matching_estimators) {
    matching_results[["nnk"]] <- lapply(PS_scores, nnk, dataset = dataset, treatment = treatment, explanatory = explanatory, ratio = ratio) 
  }
  if ("stratum" %in% matching_estimators) {
    matching_results[["stratum"]] <- tryCatch({lapply(PS_scores, stratum, dataset = dataset, treatment = treatment, explanatory = explanatory, subclass = subclass, min.n = min.n)},
                                              error=function(error_message){
                                                message(paste("Stratum matching failed; N_Obs: ", nrow(dataset), " nn_one results attached instead"))
                                                message(error_message)
                                                return(matching_results[["nn_one"]])
                                              },
                                              warning=function(error_message){
                                                message(paste("Stratum matching failed (warning); N_Obs: ", nrow(dataset), " nn_one results attached instead"))
                                                message(error_message)
                                                return(matching_results[["nn_one"]])
                                              }
    )
  }
  if ("caliper_est" %in% matching_estimators) {
    matching_results[["caliper"]] <- tryCatch({lapply(PS_scores, caliper_est, dataset = dataset, treatment = treatment, explanatory = explanatory, n_caliper = n_caliper) },
                                              error=function(error_message){
                                                message(paste("Caliper matching failed; N_Obs: ", nrow(dataset), " nn_one results attached instead"))
                                                message(error_message)
                                                return(matching_results[["nn_one"]])
                                              },
                                              warning=function(error_message){
                                                message(paste("Caliper matching failed (warning); N_Obs: ", nrow(dataset), " nn_one results attached instead"))
                                                message(error_message)
                                                return(matching_results[["nn_one"]])
                                              }
    )
    
  }
  if ("cs" %in% matching_estimators) {
    matching_results[["cs"]] <- tryCatch({lapply(PS_scores, cs, dataset = dataset, treatment = treatment, explanatory = explanatory)  },
                                         error=function(error_message){
                                           message(paste("Common support matching failed;  N_Obs: ", nrow(dataset), " nn_one results attached instead"))
                                           message(error_message)
                                           return(matching_results[["nn_one"]])
                                         },
                                         warning=function(error_message){
                                           message(paste("Common support matching failed (warning); N_Obs: ", nrow(dataset), " nn_one results attached instead"))
                                           message(error_message)
                                           return(matching_results[["nn_one"]])
                                         }
    )
  }
  return(matching_results)
}


###
# Implementation of different matching estimators
###

#One-to-one
nn_one <- function(dataset,
                   treatment, 
                   explanatory,
                   PS){
  f <- as.formula(paste(treatment, "~", paste(explanatory, collapse = "+")))
  match_log <- matchit(f, data = dataset, method = "nearest", distance = PS)
  md_log <- match.data(match_log)
  return(list(matched_data = md_log, weights = md_log$weights))
}

#One-to-many
nnk <- function(dataset,
                treatment, 
                explanatory,
                ratio,
                PS){
  f <- as.formula(paste(treatment, "~", paste(explanatory, collapse = "+")))
  match_log <- matchit(f, data = dataset, method = "nearest", distance = PS, ratio = ratio)
  md_log <- match.data(match_log)
  return(list(matched_data = md_log, weights = md_log$weights))
}

#Stratum matching
#The output of propensity score subclassification includes the assigned subclasses and 
#the subclassification weights. Effects can be estimated either within each subclass and 
#then averaged across them, or a single marginal effect can be estimated using the subclassification weights.
stratum <- function(dataset,
                    treatment, 
                    explanatory,
                    subclass,
                    min.n,
                    PS){
  f <- as.formula(paste(treatment, "~", paste(explanatory, collapse = "+")))
  match_log <- matchit(f, data = dataset, method = "subclass", distance = PS, subclass = subclass, min.n = min.n, estimand = "ATT")
  md_log <- match.data(match_log)
  return(list(matched_data = md_log, weights = md_log$weights))
}

#Caliper matching
caliper_est <- function(dataset,
                    treatment, 
                    explanatory,
                    #ratio = 2, Can also use it 
                    n_caliper, #Recommended according to some papers https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#cardinality-and-template-matching-method-cardinality
                    #Justify with papers named there, and could check some different specifications
                    PS){
  f <- as.formula(paste(treatment, "~", paste(explanatory, collapse = "+")))
  match_log <- matchit(f, data = dataset, method = "nearest", distance = PS, caliper = n_caliper, std.caliper = TRUE) #If False, caliper is in actual and not st. dev units
  md_log <- match.data(match_log)
  return(list(matched_data = md_log, weights = md_log$weights))
}

#Discarding control observations outside common support
cs <- function(dataset,
               treatment, 
               explanatory,
               PS){
  f <- as.formula(paste(treatment, "~", paste(explanatory, collapse = "+")))
  match_log <- matchit(f, data = dataset, method = "nearest", distance = PS, discard = "control") 
  #Discards all control observations outside of common support (can sometimes lead to useful control observations being discarded)
  md_log <- match.data(match_log)
  return(list(matched_data = md_log, weights = md_log$weights))
}

###
# Evaluating matching
###
# matched_data <- matching_function(PS_scores = PS_scores,
#                                   dataset = dataset,
#                                   matching_estimators = c("nn_one", "nnk", "stratum", "caliper_est", "cs"))
# TE_effects <- estimate_TE(matched_data, include_DML = FALSE)
# TE_effects
# 
# match_test <- cs(dataset = dataset, treatment = "T", explanatory = c("X1", "X2", "X3"), PS = PS_scores[[1]])
# match_test<- matchit(T~X1+X2+X3, data = dataset, method = "nearest", distance = PS_scores[[1]], discard = "control")
