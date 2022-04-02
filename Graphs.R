# Load required packages
packages <- c("ggplot2", "plyr", "MonteCarlo", "data.table")
lapply(packages, require, character.only = TRUE)

#Load packages
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
  print(ps_plot)
}
plot_PS_density(dataset)

#Plot distribution of bias
data <- MakeFrame(sim_4_result)
data_long <- melt(setDT(data), id.vars = "n_obs", variable.name = "estimators")
plot_estimate_dist <- ggplot(data_long[n_obs==500,], aes(x=value, color = estimators)) +
  geom_density()
plot_estimate_dist

#Create heatmap of variable distribution
custom_heatmap <- function(dataset, drop_vars = c("Y", "PS_scaled", "T")){
  out <- dataset %>% select(-all_of(drop_vars)) %>%
    cov() %>% 
    heatmap(Rowv = NA, Colv = NA, symm = TRUE)
  print(out)
}

