library(readxl) #package to read file
library(glmnet) #package for lasso and elastic net 
library(forecast) #package for time series forecasting
library(bestNormalize)

# Import data Brazil
brazil_indep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Brazil - Independent Variables.xlsx")
brazil_dep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Brazil - Dependent Variables.xlsx")

# Import data Costa Rica
cr_indep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Costa Rica - Independent Variables.xlsx")
cr_dep_data <- read_xlsx("C:/Users/external/Desktop/Y4B3/Seminar in ML/Coding/Data/Costa Rica - Dependent Variables.xlsx")

# Data transform
x_brazil <- as.matrix(brazil_indep_data[,-1]) 
x_cr <- as.matrix(cr_indep_data[,-1]) 
y_brazil <- as.matrix(brazil_dep_data[,-1])
y_cr <- as.matrix(cr_dep_data[,-1]) 

# Normalize Data
x_brazil <- apply(x_brazil, 2, function(col) {
  yt <- yeojohnson(col)
  return(yt$x.t)
})
# Normalize Data
x_cr <- apply(x_cr, 2, function(col) {
  yt <- yeojohnson(col)
  return(yt$x.t)
})

get_alpha_spca <- function(x, k){
  
  # Initiate alpha values for SPCA
  alpha_values_spca <- exp(seq(log(0.00001), log(1), length.out = 1000))
  msfe_values_spca <- numeric(length(alpha_values_spca))
  
  # Calculate prediction accuracy for all ridge lambdas
  for(j in seq_along(alpha_values_spca)) {
    alpha_spca <- alpha_values_spca[j]
    sparse_pca_result <- spca(X = x, k = k, alpha=alpha_spca, verbose = FALSE)
    
    factor <- sparse_pca_result$scores
    loadings <- sparse_pca_result$loadings
    estimation <- factor %*% t(loadings)
    
    mean_squared_error <- (x - estimation)^2
    mean_squared_error <- rowSums(mean_squared_error)
    mean_squared_error <- mean(mean_squared_error)
    
    msfe_values_spca[j] <- mean_squared_error
  }
  
  return(alpha_values_spca[which.min(msfe_values_spca)])
}


get_beta_spca <- function(x, k, alpha, variable_name){
  
  # Initiate alpha values for SPCA
  beta_values_spca <- exp(seq(log(0.00001), log(1), length.out = 1000))
  msfe_values_spca <- numeric(length(beta_values_spca))
  
  # Calculate prediction accuracy for all ridge lambdas
  for(j in seq_along(beta_values_spca)) {
    beta_spca <- beta_values_spca[j]
    sparse_pca_result <- spca(X = x, k = k, alpha=alpha, beta=beta_spca, verbose = FALSE)
    
    factor <- sparse_pca_result$scores
    loadings <- sparse_pca_result$loadings
    estimation <- factor %*% t(loadings)
    
    mean_squared_error <- (x - estimation)^2
    mean_squared_error <- rowSums(mean_squared_error)
    mean_squared_error <- mean(mean_squared_error)
    
    msfe_values_spca[j] <- mean_squared_error
  }
  
  return(beta_values_spca[which.min(msfe_values_spca)])
}


alpa_brazil <- get_alpha_spca(x_brazil, 2)
alpa_cr <- get_alpha_spca(x_cr, 1)
beta_brazil <- get_beta_spca(x = x_brazil, k = 2, alpha = alpa_brazil)
beta_cr <- get_beta_spca(x_cr, 1, alpa_cr)


print(alpa_brazil)
print(beta_brazil)

print(alpa_cr)
print(beta_cr)




