install.packages("MonteCarlo")
library(MonteCarlo)

#Generate y variables of length n
gen_Data <- function(nrow, ncol){
  vars <- matrix(nrow = n, ncol = y)
  vars[,1:y] <- rnorm(n, mean = 0, sd = 1)
  return(vars)
}

mean_vs_median<-function(n,scale){
  
  # generate sample
  sample<-rnorm(n, 0, scale)
  
  # calculate estimators
  mean_sample<-mean(sample)
  median_sample<-median(sample)
  
  # return results
  return(list("mean"=mean_sample, "median"=median_sample))
}

error_grid <- c(0,1)
n_grid<-c(50, 250, 500)
scale_grid<-c(1, 2, 4)

param_list=list("n"=n_grid, "scale"=scale_grid, "error" = error_grid

erg_mean_median <- MonteCarlo(func=mean_vs_median, nrep=10000, param_list=param_list)

MakeTable(output=erg_mean_median, rows="n", cols=c("scale", "list"), digits=4, include_meta=FALSE)
