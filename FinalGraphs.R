# Load required packages
packages <- c("ggpubr", "tidyverse")
lapply(packages, require, character.only = TRUE)

###
# Graph for development of estimates with different penalties
###
#Create dataset from simulation object
params <- names(sim_VM_result$param_list)
data <- MakeFrame(sim_VM_result)
data_long <- data.frame(melt(setDT(data), id.vars = params, variable.name = "estimators"))

#Subset 
est <- list(c("IPW_penalty_1", "IPW_penalty_005", "IPW_result_cv"),
            c("NIPW_penalty_1", "NIPW_penalty_005", "NIPW_result_cv"))

###
# Settings
###
title_size = 17.5
theme_update(plot.title = element_text(hjust = 0.5))
xlims = c(-1,1)
hline_size = 1.5
x_axis_ticks_size = 12
line_size = 1.3

graphs <- list()
  
#Set-up 1: IPW
data_long_subset <- data_long[data_long$estimators %in% est[[1]],] %>%
  filter(PS_formula == "PS_formula=stepNonMonotonic") %>%
  filter(n_obs == 500) %>%
  filter(X_dim == 500)

graphs[[1]] <- ggplot(data_long_subset, mapping = aes(x = value, color = estimators)) +
  geom_density(size = line_size) +
  geom_hline(aes(yintercept = 0), size = hline_size) +
  xlim(xlims) +
  ggtitle("IPW") +
  labs(y= "Quadratic  ", x = "Estimates") +
  scale_color_manual(name="Penalty",
                     labels=c("1","0.05","CV"),
                     values = c("red", "blue", "green")) +
  theme(#panel.grid.major = element_line(colour = "black", size = 0.3), 
    #panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    axis.text.x = element_text(size = x_axis_ticks_size), 
    #axis.title.x = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = title_size, face = "bold", 
                                color = "black", angle = 0, vjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = title_size, face = "bold", color = "black"))

#Set-up 1: NIPW
data_long_subset <- data_long[data_long$estimators %in% est[[2]],] %>%
  filter(PS_formula == "PS_formula=stepNonMonotonic") %>%
  filter(n_obs == 500) %>%
  filter(X_dim == 500)

graphs[[2]] <- ggplot(data_long_subset, mapping = aes(x = value, color = estimators)) +
  geom_density(size = line_size) +
  geom_hline(aes(yintercept = 0), size = hline_size) +
  xlim(xlims) +
  ggtitle("NIPW") +
  labs(y= "Quadratic", x = "Estimates") +
  scale_color_manual(name="Penalty",
                     labels=c("1","0.05","CV"),
                     values = c("red", "blue", "green")) +
  theme(#panel.grid.major = element_line(colour = "black", size = 0.3), 
    #panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    axis.text.x = element_text(size = x_axis_ticks_size), 
    #axis.title.x = element_text(size = 12),
    axis.title.x = element_blank(),
    #axis.title.y = element_text(size = 12),
    #axis.title.y = element_blank(),
    axis.title.y = element_text(size = title_size, face = "bold", color = "white", angle = 0, vjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = title_size, face = "bold", color = "black"))

#Set-Up 2 & IPW
data_long_subset <- data_long[data_long$estimators %in% est[[1]],] %>%
  filter(PS_formula == "PS_formula=linear") %>%
  filter(n_obs == 500) %>%
  filter(X_dim == 500)

graphs[[3]] <- ggplot(data_long_subset, mapping = aes(x = value, color = estimators)) +
  geom_density(size = line_size) +
  geom_hline(aes(yintercept = 0), size = hline_size) +
  xlim(xlims) +
  ggtitle("IPW") +
  labs(y= "Peak       ", x = "Estimates") +
  scale_color_manual(name="Penalty",
                     labels=c("1","0.05","CV"),
                     values = c("red", "blue", "green")) +
  theme(#panel.grid.major = element_line(colour = "black", size = 0.3), 
    #panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    axis.text.x = element_text(size = x_axis_ticks_size), 
    axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
    axis.title.y = element_text(size = title_size, face = "bold", color = "black", 
                                angle = 0, vjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = title_size, face = "bold", color = "white"))


#Set-Up 2 & NIPW
data_long_subset <- data_long[data_long$estimators %in% est[[2]],] %>%
  filter(PS_formula == "PS_formula=linear") %>%
  filter(n_obs == 500) %>%
  filter(X_dim == 500)

graphs[[4]] <- ggplot(data_long_subset, mapping = aes(x = value, color = estimators)) +
  geom_density(size = line_size) +
  geom_hline(aes(yintercept = 0), size = hline_size) +
  xlim(xlims) +
  ggtitle("NIPW") +
  labs(y= "Peak     ", x = "Estimates") +
  scale_color_manual(name="Penalty",
                     labels=c("1","0.05","CV"),
                     values = c("red", "blue", "green")) +
  theme(#panel.grid.major = element_line(colour = "black", size = 0.3), 
    #panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    axis.text.x = element_text(size = x_axis_ticks_size), 
    axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
    #axis.title.y = element_text(size = 12),
    #axis.title.y = element_blank(),
    axis.title.y = element_text(size = title_size, face = "bold", color = "white", angle = 0, vjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = title_size, face = "bold", color = "white"))


ggarrange(graphs[[1]], graphs[[2]],graphs[[3]],graphs[[4]], #labels = c("Quadratic", "", "Peak", ""),
          common.legend = TRUE, legend = "bottom", font.label = "bolt")




###
# Look at relation of mean X and PS
###

PS_formulas = c("quadraticNonSymmetric", "peakNonSymmetric")
n_obs = 1000
X_dim = 1000
impact_shares = c(0.001, 0.002, 0.004, 0.008, 0.016)#, 0.032)

#Settings
title_size = 15
theme_update(plot.title = element_text(hjust = 0.5))
x_axis_ticks_size = 12

graphs <- list()

for (PS_formula in PS_formulas){
  for (impact_share in impact_shares){
    
    #Create dataset
    PS_link = "raw"
    if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
                          "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
    dataset <- gen_DS_modular(n_obs = n_obs,
                              PS_formula = PS_formula,
                              PS_link = PS_link,
                              X_dim = X_dim,
                              X_impact_share_PS = impact_share,
                              X_impact_share_Y = impact_share)
    
    dataset$MeanX <- rowMeans(as.matrix(dataset[,1:(n_obs*impact_share)]))
    N_impact = as.character(X_dim*impact_share)
    
    #Top-left graph
    if (PS_formula == "quadraticNonSymmetric" & impact_share == 0.001) {
      graphs[[1]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
        ggtitle(paste0(N_impact)) +
        labs(y= "PS  ", x = "Mean of X") +
        geom_point() +
        scale_y_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
        scale_x_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
        theme(axis.title.y = element_text(size = title_size, face = "bold", color = "black", angle = 90),
              axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
              plot.title = element_text(size = title_size, face = "bold", color = "black"),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_))
      #Rest of top graphs
    } else if (PS_formula == "quadraticNonSymmetric") {
      graphs[[which(impact_share == impact_shares)]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
        ggtitle(paste0(N_impact)) +
        labs(y= "PS  ", x = "Mean of X") +
        geom_point() +
        scale_y_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
        scale_x_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
        theme(axis.title.y = element_text(size = title_size, face = "bold", color = "black", angle = 90),
              axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
              plot.title = element_text(size = title_size, face = "bold", color = "black"),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_)) 
   #Bottom left graph
  } else if (PS_formula == "peakNonSymmetric" & impact_share == 0.001) {
    graphs[[6]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
      ggtitle(paste0(N_impact)) +
      labs(y= "PS       ", x = "Mean of X") +
      geom_point() +
      scale_y_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
      scale_x_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
      theme(axis.title.y = element_text(size = title_size, face = "bold", color = "black", angle = 90),
            axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
            plot.title = element_text(size = title_size, face = "bold", color = "black"),
            panel.background = element_rect(fill = "transparent",
                                            colour = NA_character_)) 
    #Rest of bottom graphs
  } else if (PS_formula == "peakNonSymmetric") {
    graphs[[which(impact_share == impact_shares)+5]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
      ggtitle(paste0(N_impact)) +
      labs(y= "PS       ", x = "Mean of X") +
      geom_point() +
      scale_y_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
      scale_x_continuous(limits = c(0,1),breaks=c(0, 0.5, 1)) +
      theme(axis.title.y = element_text(size = title_size, face = "bold", color = "black", angle = 90),
            axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
            plot.title = element_text(size = title_size, face = "bold", color = "black"),
            panel.background = element_rect(fill = "transparent",
                                            colour = NA_character_)) 
  }
}
}

do.call(ggarrange, c(graphs, list(nrow = 2, ncol = 5)))




##Old:

PS_formulas = c("quadraticNonSymmetric", "peakNonSymmetric")
n_obs = 1000
X_dim = 1000
impact_shares = c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032)

#Settings
title_size = 8.5
theme_update(plot.title = element_text(hjust = 0.5))
xlims = c(-1,1)
hline_size = 1.5
x_axis_ticks_size = 12
line_size = 1.3

graphs <- list()

for (PS_formula in PS_formulas){
  for (impact_share in impact_shares){
    
    #Create dataset
    PS_link = "raw"
    if (PS_formula %in% c("flat", "linear", "quadraticSymmetric",
                          "quadraticNonSymmetric", "fourthDegree")) {PS_link = "logit"}
    dataset <- gen_DS_modular(n_obs = n_obs,
                              PS_formula = PS_formula,
                              PS_link = PS_link,
                              X_dim = X_dim,
                              X_impact_share_PS = impact_share,
                              X_impact_share_Y = impact_share)
    
    dataset$MeanX <- rowMeans(as.matrix(dataset[,1:(n_obs*impact_share)]))
    N_impact = as.character(X_dim*impact_share)
    
    #Top-left graph
    if (PS_formula == "quadraticNonSymmetric" & impact_share == 0.001) {
      graphs[[1]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
        ggtitle(paste0("V w impact: ", N_impact)) +
        labs(y= "Quadratic  ", x = "Mean of V w impact") +
        geom_point() +
        ylim(c(0,1)) +
        xlim(c(0,1)) +
        theme(axis.title.y = element_text(size = title_size, face = "bold", color = "black", angle = 0, vjust = 0.5),
              axis.title.x = element_text(size = title_size, face = "bold", color = "white"),
              plot.title = element_text(size = title_size, face = "bold", color = "black"),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_))
      #Rest of top graphs
    } else if (PS_formula == "quadraticNonSymmetric") {
      graphs[[which(impact_share == impact_shares)]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
        ggtitle(paste0("V w impact: ", N_impact)) +
        labs(y= "Quadratic  ", x = "Mean of V w impact") +
        geom_point() +
        ylim(c(0,1)) +
        xlim(c(0,1)) +
        theme(axis.title.y = element_text(size = title_size, face = "bold", color = "white", angle = 0, vjust = 0.5),
              axis.title.x = element_text(size = title_size, face = "bold", color = "white"),
              plot.title = element_text(size = title_size, face = "bold", color = "black"),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_)) 
      #Bottom left graph
    } else if (PS_formula == "peakNonSymmetric" & impact_share == 0.001) {
      graphs[[7]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
        ggtitle(paste0("V w impact: ", N_impact)) +
        labs(y= "Peak       ", x = "Mean of V w impact") +
        geom_point() +
        ylim(c(0,1)) +
        xlim(c(0,1)) +
        theme(axis.title.y = element_text(size = title_size, face = "bold", color = "black", angle = 0, vjust = 0.5),
              axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
              plot.title = element_text(size = title_size, face = "bold", color = "white"),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_)) 
      #Rest of bottom graphs
    } else if (PS_formula == "peakNonSymmetric") {
      graphs[[which(impact_share == impact_shares)+6]] <- ggplot(data = dataset, aes(x=MeanX, y = PS)) + 
        ggtitle(paste0("V w impact: ", N_impact)) +
        labs(y= "Peak       ", x = "Mean of V w impact") +
        geom_point() +
        ylim(c(0,1)) +
        xlim(c(0,1)) +
        theme(axis.title.y = element_text(size = title_size, face = "bold", color = "white", angle = 0, vjust = 0.5),
              axis.title.x = element_text(size = title_size, face = "bold", color = "black"),
              plot.title = element_text(size = title_size, face = "bold", color = "white"),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_)) 
    }
  }
}

do.call(ggarrange, c(graphs, list(nrow = 2, ncol = 6)))

