###
# Estimate variance as in Millimet & Tchernis (2009)
###
est_var <- function(dataset, PS_est, TE){
  T <- as.numeric(dataset$T)-1
  Y <- dataset$Y
  X <- dataset$X
  N <- nrow(dataset)
  pheta_vec <- ((Y*T)/PS_score) - ((Y*(1-T))/(1-PS_score)) - TE
  alpha_part_one <- (1/N)*sum((((Y*T)/PS_score^2) + ((Y*(1-T))/((1-PS_score)^2)))*X)
  alpha_part_two <- (1/N)*sum(X^2) 
  alpha_part_three <- X*(T-PS_score)
  alpha_vec <- -1*(alpha_part_one/alpha_part_two) * alpha_part_three
  V_est <- (1/N)*sum((pheta_vec+alpha_vec)^2)
  return(V_est)
}

###
# Not working
### Unsure about usage of matrix notation

# est_var_multiDim <- function(dataset, PS_score, TE){
#   T <- as.numeric(dataset$T)-1
#   Y <- dataset$Y
#   X <- as.matrix(dataset[,1:(ncol(dataset)-4)])
#   N <- nrow(dataset)
#   pheta_vec <- ((Y*T)/PS_score) - ((Y*(1-T))/(1-PS_score)) - TE
#   #alpha_part_one <- t((1/N)*sum((((Y*T)/PS_score^2) + ((Y*(1-T))/((1-PS_score)^2)))%*%as.matrix(X)))
#   alpha_part_one <- t((1/N)*(((Y*T)/PS_score^2) + ((Y*(1-T))/((1-PS_score)^2)))%*%X)
#   alpha_part_two <- (1/N)*(X%*%t(X))
#   alpha_part_three <- X*(T-PS_score)
#   alpha_vec <- -1*(alpha_part_one%/%alpha_part_two) * alpha_part_three
#   V_est <- (1/N)*sum((pheta_vec+alpha_vec)^2)
#   return(V_est)
# }


###
#Estimate coverage ratio
###
# This function should compute coverage ratios based on the estimated variance
est_confidence_interval <- function(var_est, TE, N, confidence_level = 0.975){
  Z <- qnorm(confidence_level)
  upper <- TE + Z * (var_est^0.5) / (N^0.5)
  lower <- TE - Z * (var_est^0.5) / (N^0.5)
  return(c(lower, upper))
}














# model <- Log_PS_pred(data = dataset, polynomials_vector = c(1,2,3), return_Model = TRUE)[[2]][[1]]
# predict.glm(model, newdata = grid)
# predict.glm(model, newdata = dataset[1,c(1,5)])
# 
# lasso_model <- Lasso_PS_pred(data = dataset, polynomials_vector = c(0,1,2), return_modelAndPS = TRUE)[[2]]
# predict(object = lasso_model, newx = as.matrix(grid), type = "response")
# 
# est_MISE(PS_model = lasso_model)
# est_MISE(PS_model = model)

###
# Estimate MISE
###
#cols should be set to dimension of X (excluding OneVector)
est_MISE <- function(PS_model, 
                     PS_formula = "linear", 
                     PS_link ="logit", 
                     cols = 1,  
                     X_impact_share_PS = 1){
  M = 100
  PS_model_est = PS_model[[1]]
  marker = PS_model[[2]]
  #Create grid on which estimators are evaluated on
  grid <- data.frame(matrix(rep(seq(0,1,0.01), cols), ncol = cols), rep(1,M+1)) #Also adding the "OneVector", which is dropped later
  if(ncol(grid) == 2){
    colnames(grid) = c("X", "OneVector")
  } else {colnames(grid) <- c(paste0("X", seq(1,cols,1)), "OneVector")}
  
  if (marker == "log"){
    PS_pred_est <- predict.glm(PS_model_est, newdata = grid, type = "response")
    
  } else if (marker == "lasso/ridge"){
    # Create polynomials needed for lasso (here polynomials are not passed into function but directly
    # given as covariates)
    poly = PS_model[[3]]
    if (!is.na(poly)) {
      polynomials <- matrix(nrow = 101, ncol = 0)
      for (i in poly){
        polynomials <- cbind(polynomials,grid[,-ncol(grid)]^i)}
    } else {polynomials = grid}
    
    PS_pred_est <- as.vector(predict(object = PS_model_est, newx = polynomials, type = "response"))}
  
  PS_pred_true <- gen_PS(X = as.matrix(grid[,-ncol(grid)]), impact_share = X_impact_share_PS, impact_formula = PS_formula, link_type = PS_link)
  
  MISE <- (1/M) * sum((PS_pred_est-PS_pred_true)^2)
  
  return(MISE)
}

est_SUP <- function(PS_model, 
                    PS_formula = "linear", 
                    PS_link ="logit", 
                    cols = 1,  
                    X_impact_share_PS = 1){
  M = 100
  PS_model_est = PS_model[[1]]
  marker = PS_model[[2]]
  #Create grid on which estimators are evaluated on
  grid <- data.frame(matrix(rep(seq(0,1,0.01), cols), ncol = cols), rep(1,M+1)) #Also adding the "OneVector", which is dropped later
  if(ncol(grid) == 2){
    colnames(grid) = c("X", "OneVector")
  } else {colnames(grid) <- c(paste0("X", seq(1,cols,1)), "OneVector")}
  
  if (marker == "log"){
    PS_pred_est <- predict.glm(PS_model_est, newdata = grid, type = "response")
    
  } else if (marker == "lasso/ridge"){
    # Create polynomials needed for lasso (here polynomials are not passed into function but directly
    # given as covariates)
    poly = PS_model[[3]]
    if (!is.na(poly)) {
      polynomials <- matrix(nrow = 101, ncol = 0)
      for (i in poly){
        polynomials <- cbind(polynomials,grid[,-ncol(grid)]^i)}
    } else {polynomials = grid}
    
    PS_pred_est <- as.vector(predict(object = PS_model_est, newx = polynomials, type = "response"))}
  
  PS_pred_true <- gen_PS(X = as.matrix(grid[,-ncol(grid)]), impact_share = X_impact_share_PS, impact_formula = PS_formula, link_type = PS_link)
  
  SUP <- max(abs(PS_pred_est-PS_pred_true))
  return(SUP)
}

###
# Check predictive performance of PS estimators
###

PS_predictive_performance <- function(dataset, PS_scores, printLatex = FALSE) {
  performance = lapply(PS_scores, function(i) 
    sum(ifelse(i>0.5, 1, 0) == dataset$T)/nrow(dataset))
  if(printLatex) {print(xtable(data.frame(performance), type = "latex"))}
  return(data.frame(performance))
}

###
# Check whether predicted propensity scores are constant or close to it
###

check_PS_pred_dist <- function(PS_score) {
  ifelse(max(PS_score)-min(PS_score)<0.1, 1,0)
}

