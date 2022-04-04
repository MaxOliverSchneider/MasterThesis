# Load required packages
packages <- c("MatchIt")
lapply(packages, require, character.only = TRUE)


###
# Matching function
###

matching_function <- function(PS_scores,
                              dataset,
                              treatment = "T",
                              vars_to_exclude = c("Y", "PS_scaled"),
                              matching_estimators = c("nn_one", "nnk", "stratum", "caliper", "cs"),
                              ratio = 2, #number of neighbors for knn
                              subclass = 6, #determines number of subclasses used for stratum matching
                              min.n = 1, #determines minimum of observation in subclass in stratum matching
                              caliper = 0.2) {
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
    matching_results[["stratum"]] <- lapply(PS_scores, stratum, dataset = dataset, treatment = treatment, explanatory = explanatory, subclass = subclass, min.n = min.n)
  }
  if ("caliper" %in% matching_estimators) {
    matching_results[["caliper"]] <- lapply(PS_scores, caliper, dataset = dataset, treatment = treatment, explanatory = explanatory, caliper = caliper) 
  }
  if ("cs" %in% matching_estimators) {
    matching_results[["cs"]] <- lapply(PS_scores, cs, dataset = dataset, treatment = treatment, explanatory = explanatory) 
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
caliper <- function(dataset,
                    treatment, 
                    explanatory,
                    #ratio = 2, Can also use it 
                    caliper, #Recommended according to some papers https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#cardinality-and-template-matching-method-cardinality
                    #Justify with papers named there, and could check some different specifications
                    PS){
  f <- as.formula(paste(treatment, "~", paste(explanatory, collapse = "+")))
  match_log <- matchit(f, data = dataset, method = "nearest", distance = PS, caliper = caliper, std.caliper = TRUE) #If False, caliper is in actual and not st. dev units
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
# summary(match_test_4)
# plot(match_test_4, type = "jitter", interactive = FALSE)
# plot(summary(match_test_4))
