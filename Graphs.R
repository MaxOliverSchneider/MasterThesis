#Set system language
Sys.setenv(LANG = "en")

# Load required packages
packages <- c("ggplot2", "plyr", "MonteCarlo", "data.table", "gridExtra")
lapply(packages, require, character.only = TRUE)

#Load scripts
scripts = c("GenerateData.R") 
invisible(lapply(paste0(getwd(), "/", scripts), source))

dataset <- genDS_MV_simple()

#Generate plot to compare density function of PS
plot_PS_density <- function(dataset) {
  mu <- ddply(dataset, "T", summarise, grp.mean=mean(PS_scaled))
  ps_plot <- ggplot(dataset, aes(x=PS_scaled, fill = T)) +
    geom_density(alpha = 0.4) +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=T),
               linetype="dashed")
  return(ps_plot)
}
plot_PS_density(dataset)

#Plot distribution of bias
#Probably needs some modification to show distribution of one particular set-up
dist_bias_sim_result <- function(sim_result, params =c("n_obs", "DGPS")) {
  data <- MakeFrame(sim_result)
  data_long <- melt(setDT(data), id.vars = params, variable.name = "estimators")
  plot_estimate_dist <- ggplot(data_long, aes(x=value, color = estimators)) +
    #plot_estimate_dist <- ggplot(data_long[n_obs==500,], aes(x=value, color = estimators)) +
    geom_density()  
  plot_estimate_dist
}

# data <- MakeFrame(sim_VM_result)
# data_long <- melt(setDT(data), id.vars = c("n_obs", "DGPS"), variable.name = "estimators")
# plot_estimate_dist <- ggplot(data_long[n_obs==500,], aes(x=value, color = estimators)) +
#   geom_density()
# plot_estimate_dist

#Create heatmap of variable distribution
custom_heatmap <- function(dataset, drop_vars = c("Y", "PS_scaled", "T")){
  out <- dataset %>% select(-all_of(drop_vars)) %>%
    cov() %>% 
    heatmap(Rowv = NA, Colv = NA, symm = TRUE)
  print(out)
}

#Plot estimated vs true propensity score
names(PS_scores)

plot_PS_pre <- function(dataset, PS_score, title) {
  data = data.frame(cbind(dataset$PS_scaled, PS_score))
  RMSE = round((mean((data[,1]-data[,2])^2))^0.5, digits = 2)
  MAE = round(mean(abs(data[,1] - data[,2])), digits = 2)
  colnames(data) = c("True", "Estimated")
  ggplot(data = data, aes(x = True, y = Estimated)) + 
    geom_point() + 
    ggtitle(paste0("Estimator: ", title, "; RMSE: ", RMSE, "; MAE: ", MAE))
}

plot_PS_estimator_performance <- function(dataset, PS_scores) {
  graphs <- list()
  graphs <- lapply(seq_along(PS_scores), function(i) plot_PS_pre(
    dataset = dataset, 
    PS_score = PS_scores[[i]],
    title = names(PS_scores)[[i]]))
  return(graphs)
}

output = plot_PS_estimator_performance(dataset = dataset, PS_scores = PS_scores)
do.call("grid.arrange", output)
