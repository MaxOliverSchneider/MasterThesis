#Set system language
Sys.setenv(LANG = "en")

# Load required packages
packages <- c("ggplot2", "plyr", "MonteCarlo", "data.table", "gridExtra")
lapply(packages, require, character.only = TRUE)

#Load scripts
scripts = c("GenerateData.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

#Generate plot to compare density function of PS
plot_PS_density <- function(dataset, title = "PS Density Plot", subtitle = "") {
  mu <- ddply(dataset, "T", summarise, grp.mean=mean(PS))
  ps_plot <- ggplot(dataset, aes(x=PS, fill = T)) +
    geom_density(alpha = 0.4) +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=T),
               linetype="dashed") +
    ggtitle(title, subtitle = subtitle)
  return(ps_plot)
}
###
# Plot distribution of bias for one specific set-up starting with sim_VM_result
###
#Use like: 
#plot_bias_dist(sim_result = sim_VM_result, PS_formula_p = "PS_impact=stepNonMonotonic",
#estimators = c("NIPW_log", "NIPW_true", "IPW_log", "IPW_true", "nn_one_log"))
#estimators_contain = "IPW_w_poly")
#Numeric variables like n_obs need to be given numeric and not as string, i.e. n_obs_p = 500
#IF parameter is one character string it needs to be in the form of "parameterName=parameterValue", e.g. PS_formula_p = "PS_formula=linear"
#All parameters, ven those with just one option, need to be included in the function call
#summary(sim_VM_result)
plot_bias_dist <- function(sim_result, 
                           type = "boxplot", # boxplot or density function available,
                           xlim_adjust = "automatic", 
                           cor_X_p = 0,
                           n_obs_p = 1000,
                           alpha_PS_p = 0,
                           beta_PS_p = 0,
                           DGP_p = "DGP=normal",
                           PS_link_p = "PS_link=logit",
                           PS_formula_p = "PS_impact=linear",
                           XImpact_p = "full",
                           cor_p = "nonCorrelated",
                           issue_p = "simple", 
                           impact_share_p = 0.002,
                           X_impact_share_p = 1,
                           penalty_p = 1,
                           X_dim_p = 1, 
                           red_line = -0.69,
                           estimators = NA,
                           estimators_contain = NA){
  if(xlim_adjust == "automatic"){
    xlim = c(red_line-abs(red_line),red_line+abs(red_line))
  } else {xlim = xlim_adjust}
  #If we want to show estimators across different set-ups
  # estimators <- names(data)[(length(params)+1):length(names(data))]
  # #data.frame(melt(setDT(data), id.vars = params[1:3], variable.name = "estimators", measure.vars = estimators))
  # impact_shares <- c(0.002, 0.004, 0.008, 0.016, 0.032)
  # datasets <- list()
  # for (impact_share in impact_shares){
  #   subset <- data.frame(data[data$impact_share == impact_share,])
  #   datasets <- append(datasets, subset)
  # }
  # 
  # #Merge again by repetition?
  # # [data$impact_share == 0.002]
  # 
  params <- names(sim_result$param_list)
  data <- MakeFrame(sim_result)
  data_long <- data.frame(melt(setDT(data), id.vars = params, variable.name = "estimators"))
  
  if(is.na(estimators)) {
    est <- as.vector(unique(data_long$estimators))
    if (!is.na(estimators_contain)){
      est <- as.vector(unique(data_long$estimators))[as.vector(unique(data_long$estimators)) %like% estimators_contain]
    }
  } else {est <- estimators}
  
  
  
  #Create graph, need to manually input all parameters with more than one option and
  #filter appropriate one
  data_long_subset <- data_long[data_long$estimators %in% est,] %>%
    {if("alpha_PS" %in% params) filter(., alpha_PS %in% alpha_PS_p) else . } %>%
    {if("XImpact" %in% params) filter(., XImpact %in% XImpact_p) else .} %>%
    {if("cor" %in% params) filter(., cor %in% cor_p) else .} %>%
    {if("impact_share" %in% params) filter(., impact_share %in% impact_share_p) else .} %>%
    {if("penalty" %in% params) filter(., penalty %in% penalty_p) else .} %>%
    {if("PS_formula" %in% params) filter(., PS_formula %in% PS_formula_p) else . } %>%
    {if("X_impact_share" %in% params) filter(., X_impact_share == X_impact_share_p) else . } %>%
    {if("DGP" %in% params) filter(., DGP == DGP_p) else . } %>%
    {if("issue" %in% params) filter(., issue == issue_p) else . } %>%
    {if("n_obs" %in% params) filter(., n_obs == n_obs_p) else . } %>%
    {if("PS_link" %in% params) filter(., PS_link == PS_link_p) else . } %>%
    {if("X_dim" %in% params) filter(., X_dim == X_dim_p) else . } %>%
    {if("cor_X" %in% params) filter(., cor_X == cor_X_p) else .}
  means <- aggregate(value ~  estimators, data_long_subset, mean)
  means[,2] <- round(means[,2], digits = 3)
  #Finally create plot
  #Density functions
  if(type == "density"){
    # graph <- ggplot(data_long_subset, mapping = aes(x = value, color = estimators)) +
    #   xlim(xlim) +
    #   ggtitle(as.character(penalty_p)) +
    #   geom_vline(xintercept = red_line, colour = "red") +
    #   geom_density(size = 2)
    # print(graph)}
    graph <- ggplot(data_long_subset, mapping = aes(x = value, color = estimators)) +
      geom_density(size = 1.3) +
      xlim(xlim) +
      ggtitle(paste0("DGP: ", PS_formula_p)) +
      #ggtitle(paste0("DGP: ", DGP_p)) +
      #ggtitle(paste0("Penalty: ", as.character(penalty_p))) +
      #ggtitle(paste0("N_Obs ", as.character(n_obs_p))) +
      geom_vline(xintercept = red_line, colour = "red") 
    print(graph)
    }
  
  #Boxplot
  if(type=="boxplot"){
    graph <- ggplot(data = data_long_subset, aes(x=estimators, y=value)) +
      geom_boxplot()+
      #ggtitle(paste0("DGP: ", DGP_p)) +
      ggtitle(PS_formula_p)+
      geom_hline(yintercept = red_line, colour = "red") +
      # stat_summary(fun=mean, colour="darkred", geom="point", 
      #              shape=18, size=3, show.legend=TRUE) +
      coord_flip() #+
      #geom_text(data = means, aes(label = value))}
    print(graph)
  }
  if(type=="histogram"){
    ggplot(data = data_long_subset, aes(x = value, fill = estimators)) +
      geom_histogram(alpha = 0.45)
  }
}


#Plot PS against X
# ggplot(data = data.frame(PS,X1), aes(x=X1, y=PS)) + geom_line()

####
# NOT WORKING since the param value is a string
####
# test <- function(data_long, parameter = c("alpha_PS", "cor_X"), 
#                  param_values = list(c(-0.7), c(0)))
#   data_long <- data.frame(melt(setDT(data), id.vars = params, variable.name = "estimators"))
# i = 1
#   for (param in parameter){
#     print(i)
#     print(param)
#     print(param_values[[i]])
#     data_long <- data_long %>% filter_(param %in% param_values[[i]])
#     i = i + 1
#   }

# data <- MakeFrame(sim_VM_result)
# data_long <- melt(setDT(data), id.vars = c("n_obs", "DGPS"), variable.name = "estimators")
# plot_estimate_dist <- ggplot(data_long[n_obs==500,], aes(x=value, color = estimators)) +
#   geom_density()
# plot_estimate_dist

#Create heatmap of variable distribution
custom_heatmap <- function(dataset, drop_vars = c("Y", "PS", "T")){
  out <- dataset %>% select(-all_of(drop_vars)) %>%
    cov() %>% 
    heatmap(Rowv = NA, Colv = NA, symm = TRUE)
  print(out)
}

#Plot estimated vs true propensity score

plot_PS_pre <- function(dataset, PS_score, title) {
  data = data.frame(cbind(dataset$PS, PS_score, dataset$T))
  RMSE = round((mean((data[,1]-data[,2])^2))^0.5, digits = 2)
  MAE = round(mean(abs(data[,1] - data[,2])), digits = 2)
  COR = round(cor(data[,1], data[,2]), digits = 3)
  colnames(data) = c("True", "Estimated", "Treatment")
  ggplot(data = data, aes(x = True, y = Estimated, colour = Treatment)) + 
    geom_point() + 
    ggtitle(paste0(title, "; RMSE: ", RMSE, "; MAE: ", MAE, "; COR: ", COR)) +
    theme(legend.position="none", plot.title = element_text(size=6.5))
}

plot_PS_estimator_performance <- function(dataset, PS_scores) {
  graphs <- list()
  graphs <- lapply(seq_along(PS_scores), function(i) plot_PS_pre(
    dataset = dataset, 
    PS_score = PS_scores[[i]],
    title = names(PS_scores)[[i]]))
  return(graphs)
}

plot_PS_predict_boxplot <- function(PS_scores){
  data <- data.frame(PS_scores)
  data_long <- data.frame(melt(setDT(data), variable.name = "PS"))
  ggplot(data = data_long, aes(x=PS, y=value)) +
    geom_boxplot()+
    coord_flip()
}


###
# Testing all PS estimators
###

# dataset <- gen_multiple_easy_DS(n_obs = 1000,
#                                 X_dim = 2,
#                                 PS_link = "probit",
#                                 alpha_outcome = 1,
#                                 alpha_PS = alpha_PS,
#                                 beta_PS = beta_PS,
#                                 cor_X = cor_X)
# PS_scores <- PS_estimators(dataset,
#                            estimators = c("true", "rf",
#                                           "bayes_ridge", "bayes_lasso", "bayes_horseshoe", "bayes_horseshoePlus", "nn"))
# 
# output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
# do.call("grid.arrange", c(output, top = "test")) 


