devtools::install_github("lihualei71/cfcausal")
install.packages("devtools")
install.packages("knitr")
install.packages("grf")
install.packages(c("randomForest", "gbm", "bartMachine"))
install.packages("CBPS")
install.packages("ATE")

#### Conformal inference of counterfactuals
library("cfcausal")
library("Rcpp")
library("CBPS")

# Generate data
set.seed(1)
n <- 1000
d <- 5
X <- matrix(rnorm(n * d), nrow = n)
beta <- rep(1, 5)
Y <- X %*% beta + rnorm(n) #%*% is matrix multiplication

# Generate missing indicators
missing_prob <- pnorm(X[, 1])
if_missing <- missing_prob < runif(n)
Y[if_missing] <- NA

# Generate testing data
ntest <- 5
Xtest <- matrix(rnorm(ntest * d), nrow = ntest)

# Run weighted split CQR
obj <- conformalCf(X, Y, type = "CQR", 
                   quantiles = c(0.05, 0.95),
                   outfun = "quantRF", useCV = FALSE)
predict(obj, Xtest, alpha = 0.1)

# Run weighted CQR-CV+
obj <- conformalCf(X, Y, type = "CQR", 
                   quantiles = c(0.05, 0.95),
                   outfun = "quantRF", useCV = TRUE)
predict(obj, Xtest, alpha = 0.1)
#### Conformal inference of individual treatment effects
library("cfcausal")

# Generate potential outcomes from two linear models
set.seed(1)
n <- 1000
d <- 5
X <- matrix(rnorm(n * d), nrow = n)
beta <- rep(1, 5)
Y1 <- X %*% beta + rnorm(n)
Y0 <- rnorm(n)

# Generate treatment indicators
ps <- pnorm(X[, 1])
T <- as.numeric(ps < runif(n))
Y <- ifelse(T == 1, Y1, Y0)

# Generate testing data
ntest <- 5
Xtest <- matrix(rnorm(ntest * d), nrow = ntest)

# Inexact nested method
CIfun <- conformalIte(X, Y, T, alpha = 0.1, 
                      algo = "nest", exact = FALSE, type = "CQR",
                      quantiles = c(0.05, 0.95), 
                      outfun = "quantRF", useCV = FALSE)
CIfun(Xtest)

# Exact nested method
CIfun <- conformalIte(X, Y, T, alpha = 0.1, 
                      algo = "nest", exact = TRUE, type = "CQR",
                      quantiles = c(0.05, 0.95), 
                      outfun = "quantRF",  useCV = FALSE)
CIfun(Xtest)

# Naive method
CIfun <- conformalIte(X, Y, T, alpha = 0.1, 
                      algo = "naive", type = "CQR",
                      quantiles = c(0.05, 0.95), 
                      outfun = "quantRF",  useCV = FALSE)
CIfun(Xtest)

# Counterfactual method, Y and T needs to be observed
pstest <- pnorm(Xtest[, 1])
Ttest <- as.numeric(pstest < runif(ntest))
Y1test <- Xtest %*% beta + rnorm(ntest)
Y0test <- rnorm(ntest)
Ytest <- ifelse(Ttest == 1, Y1test, Y0test)
CIfun <- conformalIte(X, Y, T, alpha = 0.1, 
                      algo = "counterfactual", type = "CQR",
                      quantiles = c(0.05, 0.95), 
                      outfun = "quantRF",  useCV = FALSE)
CIfun(Xtest, Ytest, Ttest)