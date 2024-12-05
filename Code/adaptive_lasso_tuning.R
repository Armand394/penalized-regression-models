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

#Extract dependent variables
rgdp_brazil <- y_brazil[,1]
rdgp_cr <- y_cr[,1]
cpi_brazil <- y_brazil[,2]
cpi_cr <- y_cr[,2]

#Apply log differences transformation to dependent variable (real series)
log_rgdp_brazil <- log(rgdp_brazil)
log_diff_rgdp_brazil <- diff(log_rgdp_brazil)

#Apply log differences transformation to dependent variable (real series)
log_rgdp_cr <- log(rdgp_cr)
log_diff_rgdp_cr <- diff(log_rgdp_cr)

#log second differences (price indices)
log_cpi_brazil <- log(cpi_brazil)
log_diff_cpi_brazil <- diff(log_cpi_brazil)
log_diff2_cpi_brazil <- diff(log_diff_cpi_brazil)

# Log second differences (price indices)
log_cpi_cr <- log(cpi_cr)
log_diff_cpi_cr <- diff(log_cpi_cr)
log_diff2_cpi_cr <- diff(log_diff_cpi_cr)

# Adjust size of independent variables
x_brazil_rgdp <- x_brazil[-1,]
x_cr_rgdp <- x_cr[-1,]
x_brazil_cpi <- x_brazil[-c(1, 2),]
x_cr_cpi <- x_cr[-c(1, 2),]



get_lambda_ridge <- function(x,y){
  
  # Split data into training and testing based on c_T
  T <- nrow(x)
  c_T <- ceiling(2/3 * T)
  
  # Split matrices
  train_data_x <- x[1:c_T, ]
  train_data_y <- y[1:c_T]
  test_data_x <- x[(c_T + 1):T, ]
  test_data_y <- y[(c_T + 1):T]
  
  # Initiate lambda values for ridge
  lambda_values_ridge <- exp(seq(log(0.001), log(100), length.out = 100))
  msfe_values_ridge <- numeric(length(lambda_values_ridge))
  
  # Calculate prediction accuracy for all ridge lambdas
  for(j in seq_along(lambda_values_ridge)) {
    lambda_ridge <- lambda_values_ridge[j]
    ridge_model <- glmnet(x = train_data_x, y = train_data_y, alpha = 0, lambda = lambda_ridge)
    predictions <- predict(ridge_model, s = lambda_ridge, newx = test_data_x)
    msfe_values_ridge[j] <- mean((test_data_y - predictions)^2)
  }
  
    return(lambda_values_ridge[which.min(msfe_values_ridge)])
}


get_lambda_adla <- function(x, y, lambda_ridge){
  
  ridge_model <- glmnet(x = x, y = y, alpha = 0, lambda = lambda_ridge)
  ridge_weights <- as.numeric(coef(ridge_model, s = lambda_ridge))[-1]
  
  # Split data into training and testing based on c_T
  T <- nrow(x)
  c_T <- ceiling(2/3 * T)
  
  # Split matrices
  train_data_x <- x[1:c_T, ]
  train_data_y <- y[1:c_T]
  test_data_x <- x[(c_T + 1):T, ]
  test_data_y <- y[(c_T + 1):T]
  
  # Perform Adaptive Lasso with tuned lambda
  lambda_values_adla <- exp(seq(log(0.001), log(100), length.out = 100))
  msfe_values_adla <- numeric(length(lambda_values_adla))
  
  # Calculate prediction accuracy for all adaptive lasso lambdas
  for(j in seq_along(lambda_values_adla)) {
    lambda_adla <- lambda_values_adla[j]
    adla_model <- glmnet(x = train_data_x, y = train_data_y, alpha = 1, lambda = lambda_adla, penalty.factor = 1 / abs(ridge_weights))
    predictions <- predict(adla_model, s = lambda_adla, newx = test_data_x)
    msfe_values_adla[j] <- mean((test_data_y - predictions)^2)
  }
  
  return(lambda_values_adla[which.min(msfe_values_adla)])
}


# Brazil
lambda_ridge_brazil_rgdp <- get_lambda_ridge(x_brazil_rgdp, log_diff_rgdp_brazil)
lambda_adla_brazil_rgdp <- get_lambda_adla(x_brazil_rgdp, log_diff_rgdp_brazil, lambda_ridge_brazil_rgdp)

lambda_ridge_brazil_cpi <- get_lambda_ridge(x_brazil_cpi, log_diff2_cpi_brazil)
lambda_adla_brazil_cpi <- get_lambda_adla(x_brazil_cpi, log_diff2_cpi_brazil, lambda_ridge_brazil_cpi)


# Costa Rica
lambda_ridge_cr_rgdp <- get_lambda_ridge(x_cr_rgdp, log_diff_rgdp_cr)
lambda_adla_cr_rgdp <- get_lambda_adla(x_cr_rgdp, log_diff_rgdp_cr, lambda_ridge_cr_rgdp)

lambda_ridge_cr_cpi <- get_lambda_ridge(x_cr_cpi, log_diff2_cpi_cr)
lambda_adla_cr_cpi <- get_lambda_adla(x_cr_cpi, log_diff2_cpi_cr, lambda_ridge_cr_cpi)


print(lambda_ridge_brazil_rgdp)
print(lambda_adla_brazil_rgdp)

print(lambda_ridge_brazil_cpi)
print(lambda_adla_brazil_cpi)

print(lambda_ridge_cr_rgdp)
print(lambda_adla_cr_rgdp)

print(lambda_ridge_cr_cpi)
print(lambda_adla_cr_cpi)



